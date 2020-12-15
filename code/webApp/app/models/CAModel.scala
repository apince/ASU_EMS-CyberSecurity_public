package models

import javax.inject.Inject
import edu.asu.opasced._

import scala.collection.JavaConverters._
import models.writers.JsonWriters
import play.api.libs.json.{JsValue, Json}
import com.powerdata.openpa.pwrlib.pwrflow.{ConvergenceList, FDPowerFlow}
import controllers.ModelForms.CAParams
import edu.asu.opasced.CResult.AffectedBranchInfo
import com.powerdata.openpa.core.{Bus, PAModel, PAModelException}
import java.io._
import com.powerdata.openpa.pwrlib.BasicContingencyManager.FDPFOptions
import com.powerdata.openpa.tools.PAMath

import scala.collection.mutable

class CAModel @Inject()(appmodel : AppModel, ec : JsonWriters) {
  private var _casize :Int = 0

  def saveCA() : Unit = {
    appmodel.saveConts()
  }

  def compareCA() : JsValue = {
    if(Option(appmodel.contMapBkup).nonEmpty) {
      val cres = appmodel.getCADiff
      Json.toJson( (for( (k,v) <- cres) yield {
        for( (vkey, vvalue) <- v) yield jsonCA(Map(k -> vvalue), vkey)
      }).flatten)
    } else Json.toJson(Json.obj("error" -> "not found"))
  }

  private def dtoCreator(status: String, ctyp : String, conting : String,
                 cset : List[AffectedBranchInfo], cseverity :String) =
    cset.indices.map(i => ec.CtgcyResultDTO(
      status = status,
      caseType = ctyp,
      contingency = conting,
      affected = cset(i).getBr.getID,
      preContFlow = cset(i).getPreContFlow,
      postContFlow = cset(i).getPostContFlow,
      overflowPercent = cset(i).getOverLoadPercent * 100,
      severity = cseverity))

  def jsonCA(results : Map[String, CResult], status : String): JsValue = {
    val preContMap = results.filterKeys(_=="BaseCase")

    val preContDTO = if (preContMap.nonEmpty) {
      val mset = preContMap("BaseCase").getMonitorSet.asScala.toList
      val wset = preContMap("BaseCase").getWarningSet.asScala.toList
      val vset = preContMap("BaseCase").getViolSet.asScala.toList
      dtoCreator(status, "Pre Cont.", "N/A", mset, "Monitor") ++
        dtoCreator(status, "Pre Cont.", "N/A", wset, "Warning") ++
        dtoCreator(status, "Pre Cont.", "N/A", vset, "Violation")
    } else
      Seq[ec.CtgcyResultDTO]()

    val postContMap = results.filterKeys(_!="BaseCase")
    val postContDTO =
      if(postContMap.nonEmpty) {
        (for (key <- postContMap.keys) yield {
          val mset = postContMap(key).getMonitorSet.asScala.toList
          val wset = postContMap(key).getWarningSet.asScala.toList
          val vset = postContMap(key).getViolSet.asScala.toList
          dtoCreator(status, "Post Cont.", key, mset, "Monitor") ++
            dtoCreator(status, "Post Cont.", key, wset, "Warning") ++
            dtoCreator(status, "Post Cont.", key, vset, "Violation")
        }).flatten
      } else Seq[ec.CtgcyResultDTO]()

    val cares = preContDTO ++ postContDTO
    Json.obj("results" -> Json.toJson(cares)(ec.ctgcyResultsDTOWrites)) +
      ("errorMessage" -> Json.toJson("")) + ("caSize" -> Json.toJson(_casize))
  }

  def doCA(confData : CAParams) : Map[String, CResult] = {

    val fddp = new FDPowerFlow(appmodel.model)

    val fdpfOptions = new FDPFOptions
    fdpfOptions.setConvtol(confData.convergenceTolerance.toFloat)
    fdpfOptions.setDslktol(confData.distributedSlackTolerance.toFloat)
    fdpfOptions.setnIter(confData.maxIterations)
    fdpfOptions.setSlkactol(confData.slackActivationTolerance.toFloat)
    val mlist : mutable.Buffer[Bus] = if(confData.useSlackBusList.length > 0) {
      val lbuss = confData.useSlackBusList.split("\r\n")
      appmodel.model.getBuses.asScala.filter(b => lbuss.contains(b.getID))
    } else mutable.Buffer.empty[Bus]
    if(mlist.nonEmpty)
      fdpfOptions.setSlackBus(new java.util.ArrayList[Bus](mlist.asJava))

    val v1 = PAMath.getComplexV(appmodel.model.getBuses)
    val pfRes = fddp.runPF(v1)
    fddp.updateResults(v1)

    if(pfRes.get(0).getStatus != ConvergenceList.Status.Converged) {
      Map("BaseCase power flow did not converge" -> null)
    } else {
      val largestIsle =
        appmodel.model.getElectricalIslands.asScala.filter(_.isEnergized).maxBy(i => i.getBuses.size).getIndex
      val casb : CaScedBuilder = new CaScedBuilder(appmodel.model, pfRes.getSummary)
      val cab = casb
        //CA options
        //Overload thresholds
        .baseCaseMonitorThreshold(confData.baseCaseMonitorThreshold.toFloat)
        .baseCaseWarningThreshold(confData.baseCaseWarningThreshold.toFloat)
        .baseCaseViolThreshold(confData.baseCaseViolThreshold.toFloat)
        .contCaseMonitorThreshold(confData.contCaseMonitorThreshold.toFloat)
        .contCaseWarningThreshold(confData.contCaseWarningThreshold.toFloat)
        .contCaseViolThreshold(confData.contCaseViolThreshold.toFloat)
        //Voltage violation thresholds...currently only applied to base case
        .lowVoltageWarningThreshold(confData.lowVoltageWarningThreshold.toFloat)
        .highVoltageWarningThreshold(confData.highVoltageWarningThreshold.toFloat)
        .lowVoltageViolationThreshold(confData.lowVoltageViolThreshold.toFloat)
        .highVoltageViolationThreshold(confData.highVoltageViolThreshold.toFloat)
        .selectIsland(largestIsle)
        //.build()
      if(confData.voltageFilter.toFloat.equals(0f))
        cab.setAcbPredicate(_ => true)
      else cab.setAcbPredicate(x =>
        x.getFromBus.getVoltageLevel.getBaseKV >= confData.voltageFilter.toFloat &&
          x.getToBus.getVoltageLevel.getBaseKV >= confData.voltageFilter.toFloat)

      val ca : Ca4Sced = cab.build()

      ca.setFDPFOptions(fdpfOptions)
      _casize = ca.getCset.size
      //Run CA and report
      val t1 = System.currentTimeMillis
      ca.runCA()
      val t2 = System.currentTimeMillis
      println("\nCA done in " + (t2 - t1) / 1000)

      val cresultMap = ca.getCtgcyResults.asScala.toMap
      dumpCA(cresultMap, appmodel.model, "/tmp/CConstraints.csv")
      appmodel.contingencyMap = cresultMap
      cresultMap
      }
    }

  @throws[PAModelException]
  @throws[FileNotFoundException]
  def dumpCA(csmap : Map[String, CResult], ap: PAModel, filename: String): Unit = {
    val pw = new PrintWriter(new BufferedWriter(new OutputStreamWriter(new FileOutputStream(filename))))
    pw.println("Contingency Branch,Monitored Branch,Pre-contingency flow,Post-Contingency " + "flow,Overload percent,RateC,Severity")

    for (crk <- csmap.keySet) {
      val contBranch  =
      appmodel.model.getACBranches.asScala.flatMap(_.asScala).find(br => br.getID==crk)

      if (contBranch.isEmpty && (crk != "BaseCase")) pw.println("ERROR. Key " + crk + " was not found in the results.")
      else {
        val crs = csmap(crk)
        for (abi <- crs.getMonitorSet.asScala) {
          pw.println((if (contBranch.isEmpty) "BaseCase"
          else contBranch.get.getID) + "," + abi.getBr.getID + "," + abi.getPreContFlow + "," + abi.getPostContFlow + "," + abi.getOverLoadPercent + "," + abi.getBr.getEmergencyRating + ",MON")
        }
        for (abi <- crs.getWarningSet.asScala) {
          pw.println((if (contBranch.isEmpty) "BaseCase"
          else contBranch.get.getID) + "," + abi.getBr.getID + "," + abi.getPreContFlow + "," + abi.getPostContFlow + "," + abi.getOverLoadPercent + "," + abi.getBr.getEmergencyRating + ",WAR")
        }
        for (abi <- crs.getViolSet.asScala) {
          pw.println((if (contBranch.isEmpty) "BaseCase"
          else contBranch.get.getID) + "," + abi.getBr.getID + "," + abi.getPreContFlow + "," + abi.getPostContFlow + "," + abi.getOverLoadPercent + "," + abi.getBr.getEmergencyRating + ",VIO")
        }
      }
    }
    pw.close()
  }
}