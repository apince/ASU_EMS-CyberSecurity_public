package models.writers

sealed trait ElementDTO {def id : String}

trait DataTransferObjects {
  case class PositionDTO(x: Float, y : Float)

  case class ParentNodeDTO(id : String, parent : String) extends ElementDTO

  case class TelemetryDTO(measType : String,
                          id : String,
                          measurement : Float,
                          sigma : Float,
                          isBad : Boolean,
                          isObservable:  Boolean,
                          isTelemetered : Boolean)

  case class BusDTO(id : String,
                    name : String,
                    parent : String,
                    island : String,
                    controlArea : String,
                    owner : String,
                    va : Float,
                    substation : String,
                    voltageLevel : Float,
                    totalPG : Float,
                    totalQG : Float,
                    totalPD: Float,
                    totalQD: Float,
                    totalPGDiff  : Float,
                    tlist : Seq[TelemetryDTO]
                   ) extends ElementDTO

  case class GenDTO(id : String,
                    name : String,
                    parent : String,
                    gmode : String,
                    gtype : String,
                    opMinP : Float,
                    opMaxP : Float,
                    minQ : Float,
                    maxQ : Float,
                    pSet : Float,
                    qSet : Float,
                    pg : Float,
                    qg: Float,
                    pd : Float,
                    qd : Float,
                    pgdiff  :Float,
                    isRegKV : Boolean,
                    vSet : Float,
                    regBus : String,
                    unitInAVR : Boolean,
                    isGenerating : Boolean,
                   ) extends ElementDTO

  case class LoadDTO(id : String,
                     name : String,
                     parent: String,
                     maxP : Float,
                     maxQ : Float,
                     pd : Float,
                     qd : Float,
                     pg : Float,
                     qg : Float,
                     pgdiff : Float
                    ) extends ElementDTO

  case class BranchDTO(id : String,
                       name : String,
                       source : String,
                       target : String,
                       r : Float,
                       x: Float,
                       fromTap : Float,
                       toTap : Float,
                       gmag : Float,
                       bmag  : Float,
                       fromBchg : Float,
                       toBchg : Float,
                       phaseShift : Float,
                       ltRating : Float,
                       flow  :Float,
                       fromP : Float,
                       toP : Float,
                       fromQ : Float,
                       toQ : Float,
                       tlist : Seq[TelemetryDTO]
                      ) extends ElementDTO

  case class TwoTermDevDTO(id : String,
                           name : String,
                           source : String,
                           target : String,
                           fromP : Float,
                           toP : Float,
                           fromQ : Float,
                           toQ : Float
                          ) extends ElementDTO

  case class OneTermDevDTO(id : String,
                           name : String,
                           parent: String,
                           dtype : String,
                           pg : Float,
                           qg : Float,
                           pd : Float,
                           qd : Float,
                           pgdiff : Float
                          ) extends ElementDTO


  case class Element(group : String,
                     data : ElementDTO,
                     position : Option[PositionDTO],
                     selectable : Boolean,
                     locked : Boolean,
                     grabbable : Boolean,
                     classes : String
                    )

  case class ComplexDTO(real : Float, imag : Float)

  case class BadDataDetectorDTO(probability : Float,
                                nstate : Int,
                                error : Float,
                                threshold : Float,
                                degfree : Int,
                                z0injPower : ComplexDTO)

  case class MeasRowDTO(index : Int,
                        meas : Float,
                        oldMeas : Float,
                        sigmaPU : Float,
                        isZeroInjection : Boolean,
                        residual : Float,
                        telemetryString : TelemetryDTO)

  case class PFResultsDTO (island : String ="N/A",
                           status : String ="N/A",
                           statusP : String ="N/A",
                           statusQ : String ="N/A",
                           generatedMW : Float = 0f,
                           generatedMVAr : Float = 0f,
                           loadMW : Float = 0f,
                           loadMVAr : Float =0f,
                           worstVoltageBus : String = "N/A",
                           worstVoltage : Float = 0f,
                           worstPMismatchBus : String = "N/A",
                           worstPMismatchValue: Float = 0f,
                           worstQMismatchBus : String = "N/A",
                           worstQMismatchValue : Float = 0f,
                           numIterP : Int = 0,
                           numIterQ : Int = 0,
                           maxVoltageLimit : Float = 0f,
                           minVoltageLimit : Float = 0f,
                           errorMessage : String ="")

  case class SEResultsDTO(
                           isConverged : Boolean,
                           iteration : Int,
                           norm : Float,
                           threshold : Float,
                           error: Float,
                           degFreedom : Int,
                           observableIsland : Int,
                           matrixRowCounts : Int,
                           observableRowCount : Int,
                           zeroInectionRows : Int,
                           badRows : Seq[MeasRowDTO]
                         )

  case class CtgcyResultDTO(
                             status : String,
                             caseType : String,
                             contingency : String,
                             affected : String,
                             preContFlow : Float,
                             postContFlow : Float,
                             overflowPercent : Float,
                             severity : String
                           )
}
