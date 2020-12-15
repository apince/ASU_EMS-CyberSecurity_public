package models.writers

case class CAConfig(baseCaseMonitorThreshold : Float, baseCaseWarningThreshold : Float, baseCaseViolThreshold : Float,
                    contCaseMonitorThreshold : Float, contCaseWarningThreshold : Float, contCaseViolThreshold : Float,
                    rateB : Float, rateC : Float, ratioForBaseCase : String,
                    ratioForContCase : String, lowVoltageWarningThreshold : Float,
                    highVoltageWarningThreshold : Float , lowVoltageViolThreshold : Float,
                    highVoltageViolThreshold : Float,
                    flatStart : Boolean, convergenceTolerance : Float,
                    distributedSlackTolerance : Float,
                    maxIterations : Int,
                    slackActivationTolerance : Float,
                    unitActiveLimitTolerance : Float,
                    useSlackBusList : String)