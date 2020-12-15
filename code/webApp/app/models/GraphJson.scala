package models

import javax.inject.Inject

import com.powerdata.openpa.core._
import com.powerdata.openpa.tools.PAMath
import models.ModelType.ModelType
import models.parsers.{BranchParser, BusParser, TwoTermDevParser}
import models.writers.{ElementDTO, JsonWriters}
import play.api.libs.json.Json

import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}

class GraphJson @Inject()(appmodel : AppModel)(ec : JsonWriters) {

  private def getGraphBuses(busFilterExp : String, graphType : ModelType) : Seq[Bus] = {
    lazy val buses = graphType match {
      case ModelType.NodeBreaker => appmodel.topologyModel.getBuses.asScala
      case ModelType.BusBranch => appmodel.model.getBuses.asScala
    }
    val rp1 = BusParser.parse(BusParser.expr, busFilterExp)

    if(Try(rp1.get).isSuccess)
      buses.filter(bx => BusParser.eval(rp1.get)(bx))
    else if(busFilterExp=="all") buses else Seq[Bus]()
  }

  private def nodeBreakerGraph(bnodes : Seq[Bus], filterExpr : String) = {
    val rp2 = TwoTermDevParser.parse(TwoTermDevParser.expr,filterExpr)

    if(Try(rp2.get).isSuccess) {
      val twoTermDevCollection = appmodel.topologyModel.getTwoTermDevices
        .asScala
        .flatMap(_.asScala)
        .filter(br => TwoTermDevParser.eval(rp2.get)(br))

      val forgottenNodes = twoTermDevCollection.filter(tt => !bnodes.contains(tt.getFromBus)
        || !bnodes.contains(tt.getToBus)).flatMap(tt => Seq(tt.getFromBus, tt.getToBus))
      (bnodes ++ forgottenNodes, twoTermDevCollection)
    } else {
      if(filterExpr == "all") {
        val twoTermDevCollection = appmodel.topologyModel.getTwoTermDevices.asScala
          .flatMap(_.asScala)
          .filter(tt => bnodes.contains(tt.getFromBus) && bnodes.contains(tt.getToBus))
        (bnodes, twoTermDevCollection)
      } else (bnodes, Seq[TwoTermDev]())
    }

  }

  private def busBranchGraph(bnodes : Seq[Bus], filterExpr : String) = {
    val rp2 = BranchParser.parse(BranchParser.expr,filterExpr)

    if(Try(rp2.get).isSuccess) {
      val branchCollection = appmodel.model.getACBranches.asScala
        .flatMap(_.asScala)
        .filter(br => BranchParser.eval(rp2.get)(br))

      val forgottenNodes = branchCollection
        .filterNot(tt => bnodes.contains(tt.getFromBus) && bnodes.contains(tt.getToBus))
        .flatMap(tt => Seq(tt.getFromBus, tt.getToBus)).toSet.toSeq
      (bnodes ++ forgottenNodes, branchCollection)
    } else {
      if(filterExpr=="all") {
        val branchCollection = appmodel.model.getACBranches.asScala
          .flatMap(brl => brl.asScala)
          .filter(tt => bnodes.contains(tt.getFromBus) && bnodes.contains(tt.getToBus))
        (bnodes, branchCollection)
      } else (bnodes, Seq[ACBranch]())
    }

  }

  private def getTelemList(bus : Bus) : Seq[ec.TelemetryDTO] = {
    val busInjP = bus.getTelemetry.get(TelemetryList.MeasType.BusInjP)
    val busInjQ = bus.getTelemetry.get(TelemetryList.MeasType.BusInjQ)
    val busvm = bus.getTelemetry.get(TelemetryList.MeasType.BusVM)
    Seq(
      ec.TelemetryDTO(measType = "Bus Injection P",
        id = Option(busInjP.getObject.getID).getOrElse(""),
        measurement = busInjP.getMeasurement,
        sigma = busInjP.getSigma,
        isBad = busInjP.isBadData,
        isObservable = busInjP.isObservable,
        isTelemetered = busInjP.isTelemetered),

      ec.TelemetryDTO(measType = "Bus Injection Q",
        id = Option(busInjQ.getObject.getID).getOrElse(""),
        measurement = busInjQ.getMeasurement,
        sigma = busInjQ.getSigma,
        isBad = busInjQ.isBadData,
        isObservable = busInjQ.isObservable,
        isTelemetered = busInjQ.isTelemetered),

      ec.TelemetryDTO(measType = "Bus VM",
        id = Option(busvm.getObject.getID).getOrElse(""),
        measurement = busvm.getMeasurement,
        sigma = busvm.getSigma,
        isBad = busvm.isBadData,
        isObservable = busvm.isObservable,
        isTelemetered = busvm.isTelemetered)
      )
  }

  private def getTelemList(br : ACBranch) : Seq[ec.TelemetryDTO] = {
    val fromP = br.getTelemetry.get(TelemetryList.MeasType.BranchFromP)
    val fromQ = br.getTelemetry.get(TelemetryList.MeasType.BranchFromQ)
    val toP = br.getTelemetry.get(TelemetryList.MeasType.BranchToP)
    val toQ = br.getTelemetry.get(TelemetryList.MeasType.BranchToQ)
    Seq(
      ec.TelemetryDTO(measType = "Branch_FromP",
        id = Option(fromP.getObject.getID).getOrElse(""),
        measurement = fromP.getMeasurement,
        sigma = fromP.getSigma,
        isBad = fromP.isBadData,
        isObservable = fromP.isObservable,
        isTelemetered = fromP.isTelemetered),

      ec.TelemetryDTO(measType = "Branch_FromQ",
        id = Option(fromQ.getObject.getID).getOrElse(""),
        measurement = fromQ.getMeasurement,
        sigma = fromQ.getSigma,
        isBad = fromQ.isBadData,
        isObservable = fromQ.isObservable,
        isTelemetered = fromQ.isTelemetered),

      ec.TelemetryDTO(measType = "Branch_ToP",
        id = Option(toP.getObject.getID).getOrElse(""),
        measurement = toP.getMeasurement,
        sigma = toP.getSigma,
        isBad = toP.isBadData,
        isObservable = toP.isObservable,
        isTelemetered = toP.isTelemetered),

      ec.TelemetryDTO(measType = "Branch_ToQ",
        id = Option(toQ.getObject.getID).getOrElse(""),
        measurement = toQ.getMeasurement,
        sigma = toQ.getSigma,
        isBad = toQ.isBadData,
        isObservable = toQ.isObservable,
        isTelemetered = toQ.isTelemetered)
    )
  }

  def getGraph(filterExpr : String, graphType : ModelType, groupby : String, oneTermDevs : Boolean) : String = {
    val commands = filterExpr.split("\n")

    val busNodes = getGraphBuses(commands.headOption.getOrElse(""),graphType)
    val res =
      if(graphType == ModelType.NodeBreaker)
      nodeBreakerGraph(busNodes, commands.lastOption.getOrElse(""))
    else
      busBranchGraph(busNodes, commands.lastOption.getOrElse(""))

    //position retrieval
    val pmap : Map[String, (Float, Float)] = Try(scala.io.Source.fromFile("public/" + appmodel.name+".txt")
      .getLines()) match {
      case Success(s) => (for (li <- s; vl = li.split(","))
        yield vl.head -> (vl(1).toFloat,vl(2).toFloat)).toMap
      case Failure(f) => Map[String, (Float, Float)]()
    }

    val nodes = res._1.map(b => {
      val bd = ec.BusDTO(b.getID, b.getName,
        parent = b.getStation.getID,
        b.getIsland.getID,
        b.getArea.getID,
        b.getOwner.getID,
        b.getVA,
        b.getStation.getName,
        b.getVoltageLevel.getBaseKV,
        b.getGenerators.getP.sum,
        b.getGenerators.getQ.sum,
        b.getLoads.getP.sum,
        b.getLoads.getQ.sum,
        if(Option(appmodel.dispatchDiffMap).isDefined) b.getGenerators.getID
          .withFilter(bgid => appmodel.dispatchDiffMap.keySet.contains(bgid))
          .map(bgid => appmodel.dispatchDiffMap(bgid)).sum else 0,
        getTelemList(b))
      ec.Element("nodes",
        bd,
        if(pmap.contains(bd.id)) Some(ec.PositionDTO(pmap(bd.id)._1, pmap(bd.id)._2))
        else if(pmap.contains(b.getStation.getID)) {
          val sidx = pmap(b.getStation.getID)
          Some(ec.PositionDTO(sidx._1, sidx._2))
        } else Some(ec.PositionDTO(0,0)),
        selectable = true,
        locked = false,
        grabbable = true,
        classes = "bus")
    })

    val parentNodes = groupby match {
      case "substation" => res._1.map(be =>
        ec.Element("nodes", ec.ParentNodeDTO(be.getStation.getID, be.getArea.getID), None,
          selectable = true, locked = false, grabbable = true, "substation")).distinct

      case "controlArea" => res._1.map(be =>
        ec.Element("nodes", ec.ParentNodeDTO(be.getStation.getID, be.getArea.getID), None,
          selectable = false, locked = false, grabbable = true, "substation")).distinct ++
        res._1.map(be =>
        ec.Element("nodes", ec.ParentNodeDTO(be.getArea.getID, be.getIsland.getID), None,
          selectable = false, locked = false, grabbable = true, "area")).distinct

      case "island" => res._1.map(be =>
        ec.Element("nodes", ec.ParentNodeDTO(be.getStation.getID, be.getArea.getID), None,
          selectable = false, locked = false, grabbable = true, "substation")).distinct ++
        res._1.map(be =>
          ec.Element("nodes", ec.ParentNodeDTO(be.getArea.getID, be.getIsland.getID), None,
            selectable = false, locked = false, grabbable = true, "area")).distinct ++
        res._1.map(be =>
        ec.Element("nodes", ec.ParentNodeDTO(be.getIsland.getID, ""), None,
          selectable = false, locked = false, grabbable = true, "island")).distinct

      case _ => Seq[ec.Element]()
    }

    val childNodes = if(oneTermDevs)
      appmodel.model.getOneTermDevices.asScala.flatMap(_.asScala).filter(ot => res._1.contains(ot.getBus))
        .map(ot => {
          val cd : ElementDTO  = ot.getClass.getSimpleName match {
            case "Gen" => ec.GenDTO(
              id = ot.getID,
              name = ot.getName,
              parent = ot.getBus.getID,
              gmode = ot.asInstanceOf[Gen].getMode.toString,
              gtype = "WHATEVER..FIX LATER",//ot.asInstanceOf[Gen].getType.toString,
              opMinP = ot.asInstanceOf[Gen].getOpMinP,
              opMaxP = ot.asInstanceOf[Gen].getOpMaxP,
              minQ = ot.asInstanceOf[Gen].getMinQ,
              maxQ = ot.asInstanceOf[Gen].getMaxQ,
              pSet = ot.asInstanceOf[Gen].getPS,
              qSet = ot.asInstanceOf[Gen].getQS,
              pg = ot.asInstanceOf[Gen].getP,
              qg = ot.asInstanceOf[Gen].getQ,
              pd = 0,
              qd = 0,
              pgdiff = if(Option(appmodel.dispatchDiffMap).isDefined)
                appmodel.dispatchDiffMap(ot.getID) else 0,
              isRegKV = ot.asInstanceOf[Gen].isRegKV,
              vSet = ot.asInstanceOf[Gen].getVS,
              regBus = ot.asInstanceOf[Gen].getRegBus.getID,
              unitInAVR = ot.asInstanceOf[Gen].unitInAVR,
              isGenerating = ot.asInstanceOf[Gen].isGenerating
            )
            case "Load" => ec.LoadDTO(
              id = ot.getID,
              name = ot.getName,
              parent = ot.getBus.getID,
              maxP = ot.asInstanceOf[Load].getMaxP,
              maxQ = ot.asInstanceOf[Load].getMaxQ,
              pd = -ot.asInstanceOf[Load].getP,
              qd = -ot.asInstanceOf[Load].getQ,
              pg = 0,
              qg = 0,
              pgdiff = 0
            )
            case _ => ec.OneTermDevDTO(
              id = ot.getID,
              name = ot.getName,
              parent = ot.getBus.getID,
              dtype = ot.getClass.getSimpleName,
              pg = 0,
              qg = 0,
              pd = 0,
              qd = 0,
              pgdiff = 0
            )
          }
          ec.Element("nodes",cd
            ,if(pmap.contains(cd.id))
              Some(ec.PositionDTO(pmap(cd.id)._1, pmap(cd.id)._2))
            else if(pmap.contains(ot.getBus.getStation.getID)) {
              val sidx = pmap(ot.getBus.getStation.getID)
              Some(ec.PositionDTO(sidx._1, sidx._2))
            } else Some(ec.PositionDTO(pmap(ot.getBus.getID)._1, pmap(ot.getBus.getID)._2)),
            selectable = true, locked = false, grabbable = true, "oneTermDev " + ot.getClass.getSimpleName)
        }) else Seq[ec.Element]()

    lazy val nbEdges = res._2
        .map(br => {
        val brd = ec.TwoTermDevDTO(br.getID, br.getName,
          br.getFromBus.getID,
          br.getToBus.getID,
          br.getFromP,
          br.getToP,
          br.getFromQ,
          br.getToQ
        )
        ec.Element("edges", brd, None, selectable = true,locked = false,
          grabbable = false, br.getClass.getSimpleName + " " +
          (if(br.isInService) "inService" else "outService")
          )
      })

    lazy val edges = res._2.map(br => {
        val brd = ec.BranchDTO(br.getID, br.getName,
          br.getFromBus.getID,
          br.getToBus.getID,
          br.asInstanceOf[ACBranch].getR,
          br.asInstanceOf[ACBranch].getX,
          br.asInstanceOf[ACBranch].getFromTap,
          br.asInstanceOf[ACBranch].getToTap,
          br.asInstanceOf[ACBranch].getGmag,
          br.asInstanceOf[ACBranch].getBmag,
          br.asInstanceOf[ACBranch].getFromBchg,
          br.asInstanceOf[ACBranch].getToBchg,
          br.asInstanceOf[ACBranch].getShift,
          br.asInstanceOf[ACBranch].getLTRating,
          math.max(PAMath.calcMVA(br.asInstanceOf[ACBranch].getFromP, br.asInstanceOf[ACBranch].getFromQ),
            PAMath.calcMVA(br.asInstanceOf[ACBranch].getToP, br.asInstanceOf[ACBranch].getToQ)),
          br.getFromP,
          br.getToP,
          br.getFromQ,
          br.getToQ,
          getTelemList(br.asInstanceOf[ACBranch])
        )
        ec.Element("edges", brd, None, selectable=true, locked=false, grabbable=false, br.getClass.getSimpleName)
      })

    graphType match {
      case ModelType.NodeBreaker =>
        Json.stringify(Json.toJson(childNodes ++ nodes ++ parentNodes ++ nbEdges)(ec.elementSeqWrites))
      case ModelType.BusBranch =>
        Json.stringify(Json.toJson(childNodes ++ nodes ++ parentNodes ++ edges)(ec.elementSeqWrites))
    }
  }
}
