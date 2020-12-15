package models.writers

import java.io.File

import javax.inject.Inject
import com.powerdata.openpa.core.TwoTermDev
import com.powerdata.openpa.pwrlib.pwrflow.ACBranchPowerCalc
import com.powerdata.openpa.tools.{PAMath, SimpleCSV}
import models.AppModel

import scala.collection.JavaConverters._
import scala.collection.mutable

class CsvWriters @Inject()(appmodel : AppModel) {
  private val tmpFolder = System.getProperty("java.io.tmpdir")

  private lazy val buses = appmodel.model.getBuses.asScala
  private lazy val branches : Seq[TwoTermDev] = appmodel.model.getACBranches.asScala.flatMap(acbl => acbl.asScala)
  private lazy val gens = appmodel.model.getGenerators.asScala
  private lazy val loads = appmodel.model.getLoads.asScala

  def injectMeas(iref : File , action : String)  : Array[String] = {
    action match {
      case "state" => injectState(iref)
      case "flowMeas" => injectFlowMeas(iref)
      case "injectionMeas" => injectInjectionMeas(iref)
      case "loadMeas" => injectLoadMeas(iref)
      case "dispatch" => injectDispatch(iref)
      //case "slackBusList" => injectSlackBusList(iref)
      case "decommitmentList" => injectDecommitment(iref)
      case "subgraph" => injectSubGraph(iref)
      case _ => Array[String]()
    }
  }

  def injectSubGraph(iref: File) : Array[String] = {
    val isubGraph = new SimpleCSV(iref)
    isubGraph.get("ID")
  }

  def injectDecommitment(iref : File) : Array[String] = {
    val decommitIDCsv = new SimpleCSV(iref)
    val decommitIDs = decommitIDCsv.get("ID")
    appmodel.topologyModel.getInServiceLists.asScala.foreach(isl => {
      isl.asScala.filter(ise => decommitIDs.contains(ise.getID)).foreach(_.setInService(false))
    })
    appmodel.createBusBranchModel()
    decommitIDs
  }

  def dispatchWrite() : File = {
    val flowCsv = new SimpleCSV()
    flowCsv.setHeader(Array("ID","BUS","PG","QG","PGS","QGS"))

    gens.indices.foreach(gi => {
      flowCsv.addRow()
      flowCsv.set("ID", gi, gens(gi).getID)
      flowCsv.set("BUS", gi, gens(gi).getBus.getID)
      flowCsv.set("PG", gi, gens(gi).getP)
      flowCsv.set("QG", gi , gens(gi).getQ)
      flowCsv.set("PGS", gi , gens(gi).getPS)
      flowCsv.set("QGS", gi , gens(gi).getQS)
    })
    flowCsv.save(tmpFolder+"/gens.csv")
    new File(tmpFolder+"/gens.csv")
  }

  private def injectDispatch(iref: File) : Array[String] = {
    val istate = new SimpleCSV(iref)
    val results = mutable.ArrayBuffer[String]()
    val n = istate.getRowCount

    (0 until n).foreach(i => {
      val id = istate.get("ID",i)
      val g = appmodel.model.getGenerators.getByID(id)
      scala.util.control.Exception.ignoring(classOf[NumberFormatException]) {
        val pgs = istate.getFloat("PGS", i)
        val qgs = istate.getFloat("QGS", i)

        if(Math.abs(pgs-g.getPS).compareTo(.001f) > 0 ||
          Math.abs(qgs-g.getQS).compareTo(.001f) > 0)
          results.append(id)

        g.setPS(pgs)
        g.setQS(qgs)
      }
    })

    results.toArray
  }

  def stateWrite() : File = {
    val stateCsv = new SimpleCSV()
    stateCsv.setHeader(Array("ID","VM","VA"))

    buses.indices.foreach(bi => {
      stateCsv.addRow()
      stateCsv.set(0, bi, buses(bi).getID)
      stateCsv.set(1, bi, buses(bi).getVM)
      stateCsv.set(2, bi, buses(bi).getVA)
    })
    stateCsv.save(tmpFolder+"/state.csv")
    new File(tmpFolder+"/state.csv")
  }

  private def injectState(iref: File) : Array[String] = {
    val istate = new SimpleCSV(iref)
    val n = istate.getRowCount
    val results = mutable.ArrayBuffer[String]()

    (0 until n).foreach(i => {
      val busid = istate.get("ID",i)
      val bus = appmodel.model.getBuses.getByID(busid)

      val newVM = istate.getFloat("VM",i)
      val newVA = istate.getFloat("VA",i)

      if(Math.abs(newVM-bus.getVM).compareTo(0.001f) > 0 ||
        Math.abs(newVA-bus.getVA).compareTo(0.001f) > 0)
        results.append(busid)

      bus.setVM(newVM)
      bus.setVA(newVA)
    })

    val pcv = PAMath.getComplexV(appmodel.model.getBuses)
    appmodel.model.getACBranches.asScala.foreach(acbl => {
      val apc = new ACBranchPowerCalc(acbl)
      apc.setParallel(false)
      apc.calc(pcv, (br, sf, st) => {
        results.append(br.getID)
        br.setFromP(PAMath.pu2mva(sf.re, appmodel.model.getSBASE))
        br.setToP(PAMath.pu2mva(st.re, appmodel.model.getSBASE) )
        br.setFromQ(PAMath.pu2mva(sf.im, appmodel.model.getSBASE))
        br.setToQ(PAMath.pu2mva(st.im, appmodel.model.getSBASE))
      })
    })

    appmodel.model.getBuses.asScala.foreach(bu => {
      val acbs = bu.getACBranches.asScala.flatMap(_.asScala)
      val acbf = acbs.filter(acx =>
        acx.getFromBus.getID.equals(bu.getID))
      val acbt = acbs.filter(acx =>
        acx.getToBus.getID.equals(bu.getID))
      val apf = acbf.map(_.getFromP).sum
      val aqf = acbf.map(_.getFromQ).sum
      val apt = acbt.map(_.getToP).sum
      val aqt = acbt.map(_.getToQ).sum

      bu.setMW(apf + apt)
      bu.setMVAr(aqf + aqt)
    })
    results.toArray
  }

  def flowMeasWrite() : File = {
    val flowCsv = new SimpleCSV()
    flowCsv.setHeader(Array("ID","FROM","TO","PF","PT","QF","QT"))

    branches.indices.foreach(br => {
      flowCsv.addRow()
      flowCsv.set("ID", br, branches(br).getID)
      flowCsv.set("FROM", br, branches(br).getFromBus.getID)
      flowCsv.set("TO", br, branches(br).getToBus.getID)
      flowCsv.set("PF", br , branches(br).getFromP)
      flowCsv.set("PT", br , branches(br).getToP)
      flowCsv.set("QF", br , branches(br).getFromQ)
      flowCsv.set("QT", br , branches(br).getToQ)
    })
    flowCsv.save(tmpFolder+"/flowMeasurements.csv")
    new File(tmpFolder+"/flowMeasurements.csv")
  }

  private def injectFlowMeas(iref: File) : Array[String] = {
    val istate = new SimpleCSV(iref)

    val n = istate.getRowCount
    val results = mutable.ArrayBuffer[String]()

    (0 until n).foreach(i => {
      val branchid = istate.get("ID", i)


      val branch = appmodel.model.getACBranches.asScala
        .flatMap(_.asScala).find(_.getID == branchid).get

      val newPF = istate.getFloat("PF", i)
      val newPT = istate.getFloat("PT", i)
      val newQF = istate.getFloat("QF", i)
      val newQT = istate.getFloat("QT", i)

      if(Math.abs(newPF - branch.getFromP).compareTo(0.01f) > 0 ||
        Math.abs(newPT - branch.getToP).compareTo(0.01f) > 0 ||
        Math.abs(newQF - branch.getFromQ).compareTo(0.01f) > 0 ||
        Math.abs(newQT - branch.getToQ).compareTo(0.01f) > 0)
        results.append(branchid)

      branch.setFromP(newPF)
      branch.setToP(newPT)
      branch.setFromQ(newQF)
      branch.setToQ(newQT)
    })
    results.toArray
  }

  def injectionMeasWrite() : File = {
    val injectionCsv = new SimpleCSV()
    injectionCsv.setHeader(Array("ID","PINJ","QINJ"))

    buses.indices.foreach(bi => {
      injectionCsv.addRow()
      injectionCsv.set(0, bi, buses(bi).getID)
      injectionCsv.set(1, bi, buses(bi).getGenerators.getP.sum + buses(bi).getLoads.getP.sum)
      injectionCsv.set(2, bi, buses(bi).getGenerators.getQ.sum + buses(bi).getLoads.getQ.sum)
    })
    injectionCsv.save(tmpFolder+"/injectionMeasurements.csv")
    new File(tmpFolder+"/injectionMeasurements.csv")
  }

  private def injectInjectionMeas(iref: File) : Array[String] = {
    val iInjectMeas = new SimpleCSV(iref)

    val n = iInjectMeas.getRowCount
    val results = mutable.ArrayBuffer[String]()

    (0 until n).foreach(i => {
      val busid = iInjectMeas.get("ID", i)
      val bu = appmodel.model.getBuses.getByID(busid)

      val newMW = iInjectMeas.getFloat("PINJ", i)
      val newMVar = iInjectMeas.getFloat("QINJ", i)

      if(Math.abs(newMW - bu.getMW).compareTo(0.01f) > 0 ||
        Math.abs(newMVar - bu.getMVAr).compareTo(0.01f) > 0)
        results.append(busid)

      bu.setMW(newMW)
      bu.setMVAr(newMVar)
    })
    results.toArray
  }

  def loadMeasWrite() : File = {
    val flowCsv = new SimpleCSV()
    flowCsv.setHeader(Array("ID","BUSID","PD","QD"))

    loads.indices.foreach(il => {
      flowCsv.addRow()
      flowCsv.set("ID", il, loads(il).getID)
      flowCsv.set("BUSID", il, loads(il).getBus.getID)
      flowCsv.set("PD", il, -loads(il).getP)
      flowCsv.set("QD", il, -loads(il).getQ)
    })
    flowCsv.save(tmpFolder+"/loadMeas.csv")
    new File(tmpFolder+"/loadMeas.csv")
  }

  private def injectLoadMeas(iref: File) : Array[String] = {
    val istate = new SimpleCSV(iref)
    val updateById = istate.hasCol("ID")
    val n = istate.getRowCount
    val results = mutable.ArrayBuffer[String]()
    val buslist = appmodel.model.getBuses
    val loadlist = appmodel.model.getLoads

    (0 until n).foreach(i => {
      val pd = istate.getFloat("PD",i)
      val qd = istate.getFloat("QD",i)
      if(updateById) {
        val loadid = istate.get("ID", i)
        val load = loadlist.getByID(loadid)
        if(Math.abs(load.getP + pd).compareTo(0.001f) > 0 ||
          Math.abs(load.getQ + qd).compareTo(0.00f) > 0)
          results.append(loadid)
        load.setP(-pd)
        load.setQ(-qd)
      } else {
        val busid = istate.get("BUSID",i)
        val blo  = buslist.getByID(busid)
        val nloads = blo.getLoads.size
        if(Math.abs(blo.getLoads.getP.sum + pd).compareTo(0.001f) > 0 ||
          Math.abs(blo.getLoads.getQ.sum + qd).compareTo(0.001f) > 0)
          results.append(busid)
        blo.getLoads.asScala.foreach(lo => {
          lo.setP(-pd/nloads)
          lo.setQ(-qd/nloads)
        })
      }
    })

    results.toArray
  }

}