package models

import java.io._
import java.nio.file.{Files, Paths}
import java.util.zip.{ZipEntry, ZipOutputStream}

import com.powerdata.openpa.core.Bus
import com.powerdata.openpa.core.TelemetryList.MeasType
import com.powerdata.openpa.pwrlib.BasicContingencyManager.FDPFOptions
import com.powerdata.openpa.pwrlib.pwrflow.{ConvergenceList, FDPowerFlow}
import com.powerdata.openpa.tools.{PAMath, PAModelTools}
import edu.asu.opasced.IterationSummary
import javax.inject.Inject
import models.writers.{JsonWriters, PFConfig}
import play.api.Logger
import play.api.libs.json.Json
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class PFModel @Inject()(appmodel : AppModel, ec : JsonWriters) {
  private var pf : FDPowerFlow = _

  private var isum : IterationSummary = _

  private var fdpfOptions : FDPFOptions = _

  def configPF(pfc : PFConfig) : Unit = {
    val mlist : mutable.Buffer[Bus] = if(pfc.useSlackBusList.length > 0) {
      val lbuss = pfc.useSlackBusList.split("\r\n")
      appmodel.model.getBuses.asScala.filter(b => lbuss.contains(b.getID))
    } else mutable.Buffer.empty[Bus]

    if(mlist.nonEmpty) {
      val blist = new java.util.ArrayList[Bus]()
      mlist.foreach(b => blist.add(b))
      pf = new FDPowerFlow(appmodel.model, blist)
    }
    else
      pf = new FDPowerFlow(appmodel.model)

    if(pfc.flatStart) PAModelTools.setFlatVoltage(appmodel.model)
    if(pfc.lockSVC) PAModelTools.lockSVCs(appmodel.model)
    pf.setConvergenceTolerance(pfc.convergenceTolerance)
    pf.setDistributedSlackTolerance(pfc.distributedSlackTolerance)
    pf.setMaxIterations(pfc.maxIterations)
    pf.setSlackActivationTolerance(pfc.slackActivationTolerance)
    pf.setUnitActiveLimitTolerance(pfc.unitActiveLimitTolerance)
    fdpfOptions = new FDPFOptions
    fdpfOptions.setConvtol(pfc.convergenceTolerance)
    fdpfOptions.setDslktol(pfc.distributedSlackTolerance)
    fdpfOptions.setnIter(pfc.maxIterations)
    fdpfOptions.setSlkactol(pfc.slackActivationTolerance)
    if(mlist.nonEmpty)
      fdpfOptions.setSlackBus(new java.util.ArrayList[Bus](mlist.asJava))

    if(pfc.summaryReport) {
      isum = new IterationSummary(pf.getCalculationContext, appmodel.model)
      pf.enableDebug(isum)
    }
  }

  def runPF() : String = {
    val v  = PAMath.getComplexV(appmodel.model.getBuses)
    Try(pf.runPF(v)) match {
      case Success(cvl) =>
        import com.powerdata.openpa.pwrlib.pwrflow.ConvergenceList
        if (!cvl.get(0).getStatus.equals(ConvergenceList.Status.Converged)) {
          Logger.error("Power flow did not converge")
          Logger.error("P conv: " +  cvl.get(0).getStatusP)
          Logger.error("Q conv: " +  cvl.get(0).getStatusQ)
          zipper()
          Json.stringify(
            Json.toJson(
              ec.PFResultsDTO(
                errorMessage=s"PF did not converge. If debugging was enabled, " +
                  s"you can download report files now to investigate further" +
                  s" Status: ${cvl.get(0).getStatus}, " +
                  s" Status_P: ${cvl.get(0).getStatusP}, " +
                  s" Status_Q: ${cvl.get(0).getStatusQ}, " +
                  s" Worst Voltage: ${cvl.get(0).getWorstVoltage}"
              ))
            (ec.pfResultsWrites))
        } else {
          pf.updateResults(v)
          updateInjectionMeas()
          zipper()
          jsonResults(cvl)
        }
          case Failure(err) =>
          Logger.error("Error in running Power flow")
          Logger.error(err.getStackTrace.mkString("\n"))
          Json.stringify(
            Json.toJson(
              ec.PFResultsDTO(
                errorMessage = err.getStackTrace.mkString("\n")))(ec.pfResultsWrites))
    }

  }

  def jsonResults(pfr : ConvergenceList) : String = {
    val r  : Seq[ec.PFResultsDTO] = for(i <- 0 until pfr.size(); pr = pfr.get(i))
      yield ec.PFResultsDTO(Option(pr.getIsland.getID).getOrElse(""),
        Option(pr.getStatus.toString).getOrElse(""),
        Option(pr.getStatusP.toString).getOrElse(""),
        Option(pr.getStatusQ.toString).getOrElse(""),
        appmodel.model.getGenerators.asScala.map(_.getP).sum,//pr.getGenMW,
        appmodel.model.getGenerators.asScala.map(_.getQ).sum,//pr.getGenMVAr,
        pr.getLoadMW,
        pr.getLoadMVAr,
        Option(pr.getWorstVoltage.getBus.getID).getOrElse(""),
        pr.getWorstVoltage.getVM,
        Option(pr.getWorstP.getBus.getID).getOrElse(""),
        pr.getWorstP.getValue,
        Option(pr.getWorstQ.getBus.getID).getOrElse(""),
        pr.getWorstQ.getValue,
        Option(pr.getNumIterP).getOrElse(0),
        Option(pr.getNumIterQ).getOrElse(0),
        pr.getMaxVoltageLimit,
        pr.getMinVoltageLimit)
    Json.stringify(Json.toJson(r)(ec.pfResultsSeqWrites))
  }

  private def updateInjectionMeas() : Unit = {
    appmodel.model.getBuses.asScala.foreach(b => {
      val agSum = b.getGenerators.getP.sum
      val adSum = b.getLoads.getP.sum

      val rgSum = b.getGenerators.getQ.sum
      val rdSum = b.getLoads.getQ.sum

      Option(b.getTelemetry.get(MeasType.BusInjP)) match {
        case Some(u) => u.setMeasurement(agSum + adSum)
      }

      Option(b.getTelemetry.get(MeasType.BusInjQ)) match {
        case Some(u) => u.setMeasurement(rgSum + rdSum)
      }
    })
  }

  private def zipper(): Unit = {
    if(Option(isum).isDefined) {
      //TODO: throw error if directory creation unsuccessful
      if(!Files.exists(Paths.get("/tmp/pfDetails")))
        Files.createDirectory(Paths.get("/tmp/pfDetails"))

      val odir = "/tmp/pfDetails"
      val pwf1 = new PrintWriter(new BufferedWriter(new OutputStreamWriter(
        new FileOutputStream(new File(odir, "mismatches.csv")))))
      isum.report(pwf1)
      pwf1.close()

      isum.reportDetail(new File(odir), pf.getCalculationContext.getCalc)
      isum.reportDistributedSlack(new File(odir, "distSlack.csv"), pf.getCalculationContext.getCalc)

      val pwf2 = new PrintWriter(new BufferedWriter(new OutputStreamWriter(
        new FileOutputStream(new File(odir, "swingAnalysis.csv")))))
      isum.performSwingAnalysis(pwf2)
      pwf2.close()

      //Zip the folder
      val BUFFER = 2048
      var origin : BufferedInputStream = null
      val files = Files.list(Paths.get(odir))
        .iterator().asScala

      val zipout = new ZipOutputStream(
        new BufferedOutputStream(
          new FileOutputStream("/tmp/pf.zip")))
      zipout.setMethod(ZipOutputStream.DEFLATED)
      var data = new Array[Byte](BUFFER)
      files.foreach(fis => {
        val fi = new FileInputStream(fis.toFile)
        origin = new BufferedInputStream(fi, BUFFER)
        val entry = new ZipEntry(fis.toString)
        zipout.putNextEntry(entry)

        var count: Int = 0
        while ({count = origin.read(data, 0, BUFFER);count} != -1)
          zipout.write(data, 0, count)
        origin.close()
        Files.delete(fis)
      })

      zipout.close()
    } else {
      Logger.info("No summary report for power flow. _isum is null")
    }
  }
}
