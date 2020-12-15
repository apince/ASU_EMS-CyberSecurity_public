package models.writers

case class ScedConfig(cutOffValue : Double, usePTDF : Boolean, useSparsePTDF : Boolean,
                      timeED : Double, timeSR : Double, priceSR : Double,
                      isScedModelingLMP : Boolean, loadShedPenalty : Double, useLoadShedSV : Boolean,
                      penaltyPgLBSV : Double, usePgLBSV : Boolean,
                      preventiveCtrl : Boolean , tolForMonitor : Double,
                      totalSystemImbalance : Double, autoCorrectInitSystemBalance : Boolean)