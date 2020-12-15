package controllers

import play.api.data.Forms._
import play.api.data.Form

object ModelForms {
  case class ModelParams(modelName : String, leastX : BigDecimal, decoupledZ : Boolean, 
                         genRegulateTerminal : Boolean, enableSvcVReg : Boolean,
                         showGraph : Boolean)

  case class Position(ids: Seq[String], xs: Seq[BigDecimal], ys : Seq[BigDecimal])

  case class PFParams(flatStart : Boolean, lockSVC : Boolean,
                      convergenceTolerance : BigDecimal,
                      distributedSlackTolerance : BigDecimal,
                      maxIterations : Int, slackActivationTolerance : BigDecimal,
                      unitActiveLimitTolerance : BigDecimal,
                      summaryReport : Boolean,
                      useSlackBusList : String)

  case class SEParams(convergenceTolerance : BigDecimal,
                      maxIterations : Int,
                      scaleNoise : BigDecimal,
                      injectAttack : Boolean,
                      attackRound : Int)

  case class CAParams(baseCaseMonitorThreshold : BigDecimal, baseCaseWarningThreshold : BigDecimal, baseCaseViolThreshold :BigDecimal,
    contCaseMonitorThreshold : BigDecimal, contCaseWarningThreshold :BigDecimal, contCaseViolThreshold :BigDecimal,
                      lowVoltageWarningThreshold : BigDecimal,
                      highVoltageWarningThreshold : BigDecimal , lowVoltageViolThreshold : BigDecimal,
                      highVoltageViolThreshold : BigDecimal,
                      flatStart : Boolean, convergenceTolerance : BigDecimal,
                      distributedSlackTolerance : BigDecimal,
                      maxIterations : Int, slackActivationTolerance : BigDecimal,
                      unitActiveLimitTolerance : BigDecimal,
                      useSlackBusList : String,
                      voltageFilter : BigDecimal)

  case class MonitorParams(simAllPotentialContingecy : Boolean,
                           monintorALLBrc4SimAllPotentialCtgcy : Boolean,
                           monitorAllBrcBaseCase : Boolean,
                           monitorAllBrcCtgcyCase : Boolean,
                           usePkInit : Boolean,
                           usePkcInit : Boolean,
                           cutOffValue : BigDecimal,
                           usePTDF : Boolean,
                           useSparsePTDF : Boolean,
                           timeED : BigDecimal,
                           timeSR : BigDecimal,
                           priceSR : BigDecimal,
                           isScedModelingLMP : Boolean,
                           optionLoss: Int)

  case class SlackParams(loadShedPenalty : BigDecimal,
                         useLoadShedSV : Boolean,
                         penaltyPgLBSV : BigDecimal,
                         usePgLBSV : Boolean,
                         preventiveCtrl : Boolean ,
                         tolForMonitor : BigDecimal,
                         totalSystemImbalance : BigDecimal,
                         autoCorrectInitSystemBalance : Boolean,
                         useSlackBranch : Boolean,
                         slackBranchPenalty: BigDecimal,
                         useSavedCtgcy : Boolean,
                         createReport : Boolean,
                         deratings : String)

  case class ScedParams(monitorParams: MonitorParams, slackParams: SlackParams)

  val paramForm = Form(
    mapping(
      "modelName" -> default(nonEmptyText,"Polish"),
      "leastX" -> default[BigDecimal](bigDecimal(5 ,5), 0.0001),
      "decoupledZ" -> boolean,
      "genRegulateTerminal" -> boolean,
      "enableSvcVReg" -> boolean,
      "showGraph" -> boolean
    )(ModelParams.apply)(ModelParams.unapply)
  )

  val positionForm = Form(
    mapping(
      "ids" -> seq(text),
      "xs" ->  seq(bigDecimal),
      "ys" -> seq(bigDecimal),
    )(Position.apply)(Position.unapply)
  )

  val pfParamForm = Form(
    mapping(
      "flatStart" -> default[Boolean](boolean, false),
      "lockSVC" -> default[Boolean](boolean, false),
      "convergenceTolerance" -> default[BigDecimal](bigDecimal(5, 5), 0.5),
      "distributedSlackTolerance" -> default[BigDecimal](bigDecimal(5, 5), 0.5),
      "maxIterations" -> default[Int](number, 40),
      "slackActivationTolerance" -> default[BigDecimal](bigDecimal(5, 2), 50.0),
      "unitActiveLimitTolerance" -> default[BigDecimal](bigDecimal(5, 5), 0.5),
      "summaryReport" -> default[Boolean](boolean, false),
      "useSlackBusList" -> default[String](text, "")
    )(PFParams.apply)(PFParams.unapply)
  )

  val seParamForm = Form(
    mapping(
      "convergenceTolerance" -> default[BigDecimal](bigDecimal(5, 5), 0.5),
      "maxIterations" -> default[Int](number, 40),
      "scaleNoise" -> default[BigDecimal](bigDecimal(4, 2), 0.50),
      "injectAttack" -> default(boolean, false),
      "attackRound" -> default[Int](number, 1)
    )(SEParams.apply)(SEParams.unapply)
  )

  val caParamForm = Form(
    mapping(
      "baseCaseMonitorThreshold" -> default[BigDecimal](bigDecimal(3,2), 0.90),
      "baseCaseWarningThreshold" -> default[BigDecimal](bigDecimal(3,2), 0.90),
      "baseCaseViolThreshold" -> default[BigDecimal](bigDecimal(3,2), 1.00),
      "contCaseMonitorThreshold" -> default[BigDecimal](bigDecimal(3,2), 0.90),
      "contCaseWarningThreshold" -> default[BigDecimal](bigDecimal(3,2), 0.90),
      "contCaseViolThreshold" -> default[BigDecimal](bigDecimal(3,2), 1.00),
      "lowVoltageWarningThreshold" -> default[BigDecimal](bigDecimal(3,2), 0.95),
      "highVoltageWarningThreshold" -> default[BigDecimal](bigDecimal(3,2), 1.05),
      "lowVoltageViolThreshold" -> default[BigDecimal](bigDecimal(3,2), 0.90),
      "highVoltageViolThreshold" -> default[BigDecimal](bigDecimal(3,2), 1.10),
      "flatStart" -> default[Boolean](boolean, false),
      "convergenceTolerance" -> default[BigDecimal](bigDecimal(5, 5), 0.5),
      "distributedSlackTolerance" -> default[BigDecimal](bigDecimal(5, 5), 0.5),
      "maxIterations" -> default[Int](number, 40),
      "slackActivationTolerance" -> default[BigDecimal](bigDecimal(5, 2), 50.0),
      "unitActiveLimitTolerance" -> default[BigDecimal](bigDecimal(5, 5), 0.5),
      "useSlackBusList" -> default[String](text, ""),
      "voltageFilter" -> default[BigDecimal](bigDecimal(5,1), 100.0)
    ) (CAParams.apply)(CAParams.unapply)
  )

  val scedMonitorParamForm =
    mapping(
      "simAllPotentialContingecy" -> default(boolean, false),
      "monintorALLBrc4SimAllPotentialCtgcy" -> default(boolean, true),
      "monitorAllBrcBaseCase" -> default(boolean, false),
      "monitorAllBrcCtgcyCase" -> default(boolean, false),
      "usePkInit" -> default(boolean, true),
      "usePkcInit" -> default(boolean, true),
      "cutOffValue" -> default[BigDecimal](bigDecimal(5,5), 0.00010),
      "usePTDF" -> default(boolean, true),
      "useSparsePTDF" -> default(boolean, false),
      "timeED" -> default[BigDecimal](bigDecimal(5,2), 15),
      "timeSR" -> default[BigDecimal](bigDecimal(5,2), 10),
      "priceSR" -> default[BigDecimal](bigDecimal(5,2), 699),
      "isScedModelingLMP" -> default(boolean, true),
      "optionLoss" -> default[Int](number, 2)
    )(MonitorParams.apply)(MonitorParams.unapply)


  val scedSlackParamForm =
    mapping(
      "loadShedPenalty" -> default[BigDecimal](bigDecimal(10,0), 10e7),
      "useLoadShedSV" -> default(boolean, false),
      "penaltyPgLBSV" -> default[BigDecimal](bigDecimal(10,0), 5e7),
      "usePgLBSV" -> default(boolean, false),
      "preventiveCtrl" -> default(boolean, true),
      "tolForMonitor" -> default[BigDecimal](bigDecimal(5,5), 0),
      "totalSystemImbalance" -> default[BigDecimal](bigDecimal(3,3),0.050),
      "autoCorrectInitSystemBalance" -> default(boolean, false),
      "useSlackBranch" -> default(boolean, false),
      "slackBranchPenalty" -> default[BigDecimal](bigDecimal(10, 0), 10e7),
      "useSavedCtg" -> default(boolean, false),
      "createReport" -> default(boolean, false),
      "deratings" -> default[String](text,
        "ln-3022-3033-1,1\nln-6188-7305-1,0.97\nln-7233-7251-1,0.97")

  )(SlackParams.apply)(SlackParams.unapply)

  val scedParamForm : Form[ScedParams] = Form(
    mapping(
      "monitorParams" -> scedMonitorParamForm,
      "slackParams" -> scedSlackParamForm)
    (ScedParams.apply)(ScedParams.unapply)
  )

}