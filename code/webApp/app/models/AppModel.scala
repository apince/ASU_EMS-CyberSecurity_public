package models

import com.mathworks.engine.MatlabEngine
import javax.inject.Singleton
import com.powerdata.openpa.core._
import models.writers.PAModelConfig
import com.powerdata.openpa.pwrlib.BusBranchModelBldr
import com.powerdata.openpa.pwrlib.BusBranchModelBldr.Topology
import com.powerdata.openpa.tools.PAModelTools
import edu.asu.opasced.CResult
import play.api.Logger

import scala.collection.JavaConverters._

@Singleton
class AppModel() {

  type ContingencyMap= Map[String, CResult]

  private var _m: PAModel = _
  private var _model : PAModel = _
  private var _name = "Polish"
  private var _loads : Map[String, (Float, Float)] = _

  private var _contingencyMap: ContingencyMap = _
  private var _dispatchDiffMap : Map[String, Float] = _
  private var _dispatchMap : Map[String, Float] = _
  private val _matlabEng = MatlabEngine.startMatlab()
  _matlabEng.eval("cd AttackDetection")
  _matlabEng.eval("addpath('PolishData')")
  _matlabEng.eval("addpath('TexasData')")

  def loads : Map[String, (Float, Float)] = _loads

  def matlabEng:MatlabEngine = {
    _matlabEng
  }
  //hold on to dispatch values
  def saveDipatch() : Unit = {
    _dispatchMap = _model.getGenerators.asScala.map(g => g.getID -> g.getPS).toMap
  }

  //upload current dispatches with stored dispatch
  def retrieveDispatch() : Unit = {
    _model.getGenerators.asScala.foreach(g =>
      if(_dispatchMap.contains(g.getID)) g.setPS(_dispatchMap(g.getID)))
  }

  //hold on to the contingency map
  private var _contMapBkup : ContingencyMap = _

  def saveConts() : Unit = {
    if(Option(_contingencyMap).nonEmpty)
      _contMapBkup = for( (k,v) <- _contingencyMap) yield k -> v
  }

  def loadBkupCtgcy() : Unit = {
    _contingencyMap = Map(_contMapBkup.toSeq: _*)
  }

  def contMapBkup: ContingencyMap = _contMapBkup

  def getCADiff: Map[String, ContingencyMap] = {
    (for( (k, v) <- _contingencyMap) yield {
      if(_contMapBkup.keySet.contains(k)) k -> Map("Saved" -> _contMapBkup(k), "Current" -> v)
      else k -> Map("Current" -> v)
    }) ++ (for( (k, v) <- _contMapBkup if !_contingencyMap.keySet.contains(k))
      yield k -> Map("Saved" -> v))
  }

  def updateContgys(ctids : Array[String]) : Boolean = {
    val ctgys = ctids
      .map(xs => {
        val spl = xs.split("___")
        if(spl.head == "N/A")
          Seq("BaseCase", spl.last)
        else
          Seq(spl.head, spl.last)
      })

    val tmpMap = _contingencyMap.collect {
      case cont : (String, CResult) =>
        val foundInRemoval = ctgys.filter(_.head == cont._1)
        if (foundInRemoval.nonEmpty) {
          val eligibleAffecteds = foundInRemoval.map(_.last)
          val cr2 = new CResult()
          cont._2.getMonitorSet.stream()
            .filter(af => !eligibleAffecteds.contains(af.getBr.getID))
            .forEach(af => cr2.addToMonitorSet(af))
          cont._2.getWarningSet.stream()
            .filter(af => !eligibleAffecteds.contains(af.getBr.getID))
            .forEach(af => cr2.addToWarningSet(af))
          cont._2.getViolSet.stream()
            .filter(af => !eligibleAffecteds.contains(af.getBr.getID))
            .forEach(af => cr2.addToViolSet(af))
          (cont._1, cr2)
      } else cont
    }

    val prevSize = _contingencyMap.map(x => x._2.contSize()).sum
    _contingencyMap = tmpMap
    val newSize = _contingencyMap.map(x => x._2.contSize()).sum
    _contMapBkup = null
    if(newSize == prevSize - ctgys.length)
      true
    else
      false
  }

  def reset() : Unit = {
    _m = null
    _model = null
    _name = null
    _contingencyMap = null
    _dispatchDiffMap = null
    _loads = null
  }

  def contingencyMap: Map[String, CResult] = _contingencyMap

  def contingencyMap_=(value: Map[String, CResult]): Unit = {
    _contingencyMap = value
  }

  def dispatchDiffMap : Map[String, Float] = _dispatchDiffMap

  def dispatchDiffMap_= (value : Map[String, Float]) : Unit = {
    _dispatchDiffMap = value
  }

  def config(pamConfig : PAModelConfig) : Unit = {
    val bldr: PflowModelBuilder = PflowModelBuilder.Create(pamConfig.uri)
    _name = pamConfig.name
    bldr.setLeastX(pamConfig.leastX)
    bldr.decoupleImpedance(pamConfig.decoupledZ)
    bldr.setUnitRegOverride(pamConfig.genRegulateTerminal)
    bldr.setSvcVRegEnabled(pamConfig.enableSvcVReg)
    _m  = bldr.load
    PAModelTools.setDeEnergizedOOS(_m)
    createBusBranchModel()
  }

  def createBusBranchModel() : Unit = {
    //Turn off all other islands except the largest island
    val largestIsle =
      _m.getElectricalIslands.asScala.filter(_.isEnergized).maxBy(i => i.getBuses.size).getIndex
    _m.getElectricalIslands.asScala.filter(_.getIndex!=largestIsle).foreach {ei =>
      if(ei.isEnergized)
        Logger.warn("Warning! " +
          "Energized electrical island other than the largest island found..." +
        " Turning off extra island.")
      ei.getInServiceLists.asScala.foreach(isl => isl.asScala.foreach(isd => isd.setInService(false)))
    }
    _model = new BusBranchModelBldr(_m, Topology.SingleBus).load
    _loads = _model.getLoads.asScala.map(lo => lo.getID -> (lo.getP, lo.getQ)).toMap
  }

  def name : String = _name

  def model : PAModel = Option(_model) match {
    case Some(model) => model
    case None => createBusBranchModel(); _model
  }

  def topologyModel : PAModel = _m

  def setService(id : String, command : Boolean) : Unit = {

    lazy val elemTwoTerm = _m.getTwoTermDevices.asScala.flatMap(tt => tt.asScala).find(tt => tt.getID == id)

    lazy val elemOneTerm = _m.getOneTermDevices.asScala.flatMap(tt => tt.asScala).find(tt => tt.getID == id)

    if(elemTwoTerm.isDefined) elemTwoTerm.get.setInService(command)
    else if(elemOneTerm.isDefined) elemOneTerm.get.setInService(command)
  }

}
