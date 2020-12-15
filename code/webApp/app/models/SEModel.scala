package models


import javax.inject.Inject
import com.powerdata.openpa.core._
import com.powerdata.openpa.pwrlib.se.MeasurementCaseModelBuilder
import com.powerdata.openpa.pwrlib.se.MeasMgr
import com.powerdata.openpa.pwrlib.se.ObservableIslandList
import com.powerdata.openpa.pwrlib.se.StateEstimator
import com.powerdata.openpa.tools.PAMath
import models.writers.{JsonWriters, SEConfig}
import com.powerdata.openpa.core.TelemetryList.MeasType
import com.powerdata.openpa.core.Bus
import com.powerdata.openpa.tools.PAModelTools
import java.io._

import com.mathworks.engine._
import play.api.libs.json.{JsObject, Json}

import scala.collection.JavaConverters._

class SEModel @Inject()(appmodel : AppModel, ec : JsonWriters) {
  private var measCase = appmodel.model
  private lazy val se : StateEstimator = new StateEstimator(measCase, appmodel.model)
  private var seResults : Seq[StateEstimator#SEResults] = Seq[StateEstimator#SEResults]()
  private var noiseScale : Float = 0

  private def configureSigmaZ() : Unit = {
    val nonInjBuses = PAModelTools.getZeroInjectionBuses(measCase)
    nonInjBuses.asScala.foreach(b => {
      val t = b.getTelemetry
      t.get(MeasType.BusInjP).setSigma(1e-4f)
      t.get(MeasType.BusInjQ).setSigma(1e-4f)
    })
  }

  def config(sfc : SEConfig) : Unit = {
    if(sfc.scaleNoise.compareTo(0f) > 0) {
      measCase =
        new MeasurementCaseModelBuilder(appmodel.model).applyNoise(sfc.scaleNoise).load
      noiseScale = sfc.scaleNoise
//      for (col <- ColumnMeta.values) {
//        val list: BaseList[_ <: BaseObject] = measCase.getList(col.getListType)
//        list.getIndexes // throw away results, just make use of the fact that this will force the data to be actually loaded.
//      }
    }

    configureSigmaZ()
    se.setConvergenceTolerance(sfc.convergenceTolerance)
    se.setMaxIterations(sfc.maxIterations)
  }

  def runSE() : String = {
    val v = PAMath.getComplexV(appmodel.model.getBuses)

    implicit val pwx: PrintWriter = new PrintWriter(new BufferedWriter(new OutputStreamWriter(
      new FileOutputStream("/tmp/seresults.csv"))))

    pwx.print("ObservableIsland,Row,ID,Type,FromBus,ToBus,CaseMeas,NoisyMeas,Estimated,")
    pwx.print("isTelemetered,Sigma,Residual,isBad,isObservable,isZeroInjection,sumBranchSigma,")
    pwx.println("CaseCompare,Units")
    seResults = se.runSE(v, reportAndUpdate).asScala
    pwx.close()
    "No Console MSG"
  }

  //helpers
  private def oldMeasFinderTD(td : TwoTermDev, mtype : String) = mtype match {
    case "BranchFromP" => td.getFromP
    case "BranchFromQ" => td.getFromQ
    case "BranchToP" => td.getToP
    case "BranchToQ" => td.getToQ
  }

  private def oldMeasFinderBU(od : Bus, mtype : String) = mtype match {
    case "BusInjP" => od.getMW
    case "BusInjQ" => od.getMVAr
    case "BusVM" => od.getVM
  }


  def findbyIdInModel(id : String, measType : String) : Float = {
    val tid = appmodel.model.getTwoTermDevices.asScala.collect {
      case x if x.asScala.map(y => y.getID).contains(id) => oldMeasFinderTD(x.getByID(id), measType)
    }
    if(tid.isEmpty)
      appmodel.model.getBuses.asScala.collect {
        case x if x.getID == id => oldMeasFinderBU(x, measType)
      }.head
    else tid.head
  }

  def jsonResults() = {
    //TODO: replace true in SESearchResultDTO with actual convergence
    val serc = seResults.map(sr =>
      ec.SEResultsDTO(true,
        sr.getIteration,
        sr.getNorm,
        sr.getBadData.getThreshold,
        sr.getBadData.getError,
        sr.getBadData.getDegreesOfFreedom,
        sr.getIsland.getIndex,
        sr.getMatrixRowCount,
        sr.getObservableRowCount,
        sr.getZeroInjectionRows,
        sr.getBadRows.asScala.map(r => ec.MeasRowDTO(
          index = r.getIndex,
          meas = r.getMeasurementPU,
          oldMeas = findbyIdInModel(r.getTelemetry.getObject.getID, r.getTelemetry.getMeasType.toString),
          sigmaPU = r.getSigmaPU,
          isZeroInjection = r.isZeroInjection,
          residual = r.getResidual,
          telemetryString = ec.TelemetryDTO(
            measType = r.getTelemetry.getMeasType.toString,
            id = Option(r.getTelemetry.getObject.getID).getOrElse(r.getIndex.toString),
            measurement = r.getTelemetry.getMeasurement,
            sigma = r.getTelemetry.getSigma,
            isBad = r.getTelemetry.isBadData,
            isObservable = r.getTelemetry.isObservable,
            isTelemetered = r.getTelemetry.isTelemetered
          )
        ))
    ))
    se.updateResults()
    se.cleanup()

    val loadList: Array[Double] =
    if(appmodel.name.equalsIgnoreCase("polish"))
      appmodel.model.getLoads.asScala
      .filter(load => !(load.getID == "load-176-1" ||
        load.getID == "load-177-1" ||
        load.getID == "load-1980-1" ||
             load.getID == "load-2153-1"))
    .map(-_.getP.toDouble).toArray
    else if(appmodel.name.equalsIgnoreCase("activsg"))
      appmodel.model.getBuses.asScala
      .collect {
        case bus if bus.getLoads.size > 0 =>
          -bus.getLoads.getP.sum.toDouble
      }.toArray
    else Array()

//    val writer = new PrintWriter(new File("WriteLoad.csv"))
//
//    loadList.foreach(xv => writer.println(xv))
//    writer.close()

    val cmdet = loadTesterNN(loadList, appmodel.name)
    //val cmdet = Array(1,2,3,4,5,6)
    val js1 : JsObject = JsObject(Seq("NNDetector" -> Json.toJson(cmdet)))
    val js2: JsObject =
      JsObject(Seq("CHiSqDetector" -> Json.toJson(serc)(ec.seResultsSeqWrites)))
    js1 ++ js2
  }

  private def reportAndUpdate(island : ObservableIslandList#ObservableIsland ,
                     m : PAModel, row : MeasMgr#Row, est : Float, base : Float)
                             (implicit pw : PrintWriter) : Unit = {
    var fbus : Bus= null
    var tbus : Bus = null
    val t = row.getTelemetry
    val typ = t.getMeasType
    val bo = t.getObject

    if (classOf[ACBranch].isInstance(bo)) {
      val br = bo.asInstanceOf[ACBranch]
      fbus = br.getFromBus
      tbus = br.getToBus
      val br2 = measCase.getACBranches.asScala.flatMap(_.asScala).find(_.getID==bo.getID).head
      br2.getTelemetry.get(typ).setBadData(t.isBadData)
      br2.getTelemetry.get(typ).setObservable(t.isObservable)
      br2.getTelemetry.get(typ).setTelemetered(t.isTelemetered)
    } else if (classOf[Bus].isInstance(bo)) {
      fbus = bo.asInstanceOf[Bus]
      val fbus2 = measCase.getBuses.getByID(bo.getID)
      fbus2.getTelemetry.get(typ).setBadData(t.isBadData)
      fbus2.getTelemetry.get(typ).setObservable(t.isObservable)
      fbus2.getTelemetry.get(typ).setTelemetered(t.isTelemetered)
    }

    val rowx = row.getIndex
    var otherSigma : Float = 0f

    if(typ==MeasType.BusInjP || typ==MeasType.BusInjQ) {
      val isp = typ == MeasType.BusInjP
      val bus = bo.asInstanceOf[Bus]
      bus.getACBranches.asScala.flatMap(_.asScala).foreach(branch => {
        val isfrom = branch.getFromBus.equals(bus)
        val brtyp = if(isp) {
          if(isfrom) MeasType.BranchFromP else MeasType.BranchToP
        } else {
          if(isfrom) MeasType.BranchFromQ else MeasType.BranchToQ
        }
        otherSigma += branch.getTelemetry.get(brtyp).getSigma
      })
    }

    var comp = 0f
    var units = ""
    typ match {
      case MeasType.BusVM =>
        comp = PAMath.pu2vm(bo.asInstanceOf[Bus], Math.abs(est-base))
        units = "KV"

      case MeasType.BranchFromP | MeasType.BranchToP | MeasType.BusInjP =>
        units = "MW"
        comp = PAMath.pu2mva(Math.abs(est - base), m.getSBASE)

      case MeasType.BranchFromQ | MeasType.BranchToQ | MeasType.BusInjQ =>
        units = "MVAr"
        comp = PAMath.pu2mva(Math.abs(est - base), m.getSBASE)
    }
    pw.format("%d,%d,%s,%s,%s,%s,%f,%f,%f,%s,%f,%f,%s,%s,%s,%s,%f,%s\n",
      int2Integer(island.getIndex),
      int2Integer(rowx),
      t.getObject.getID,
      typ.toString,
      if (Option(fbus).isDefined) fbus.getID else "" ,
      if (Option(tbus).isDefined) tbus.getID else "" ,
      float2Float(base),
    float2Float(row.getMeasurementPU),
      float2Float(est),
    String.valueOf(t.isTelemetered),
    float2Float(row.getSigmaPU),
    float2Float(row.getResidual),
    String.valueOf(t.isBadData),
    String.valueOf(t.isObservable),
    String.valueOf(row.isZeroInjection),
    if(Option(otherSigma).isDefined) float2Float(PAMath.mva2pu(otherSigma, m.getSBASE)) else float2Float(0f),
      float2Float(comp),
    units
    )
  }

  def loadTesterNN(loads: Array[Double], modelName: String): Array[Double] = {
    val res: Array[Object] =
    if(modelName.equalsIgnoreCase("activsg"))
      appmodel.matlabEng.feval(6,"NearestNeighborDetection", loads, "t")
    else
      appmodel.matlabEng.feval(6,"NearestNeighborDetection", loads, "p")
    //eng.close()
    res.map(r => r.asInstanceOf[Double])
  }
}
