package models.writers

case class PFConfig(flatStart : Boolean, lockSVC : Boolean,
                    convergenceTolerance : Float,
                    distributedSlackTolerance : Float,
                    maxIterations : Int,
                    slackActivationTolerance : Float,
                    unitActiveLimitTolerance : Float,
                    summaryReport : Boolean,
                    useSlackBusList : String)