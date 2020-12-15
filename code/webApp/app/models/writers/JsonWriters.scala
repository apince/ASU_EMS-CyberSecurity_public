package models.writers


import play.api.libs.json.Json.JsValueWrapper
import play.api.libs.json._


class JsonWriters extends DataTransferObjects {

  implicit val positionWrites = new Writes[PositionDTO] {
    def writes(positionDTO: PositionDTO): JsObject = Json.obj(
      "x" -> positionDTO.x,
      "y" -> positionDTO.y
    )
  }

  implicit val telemetryWrites = new Writes[TelemetryDTO] {
    def writes(telemetryDTO: TelemetryDTO): JsValue = Json.obj(
      "ID" -> telemetryDTO.id,
      "Type" -> telemetryDTO.measType,
      "Value" -> telemetryDTO.measurement,
      "Sigma" -> telemetryDTO.sigma,
      "Telemetered" -> (if(telemetryDTO.isTelemetered) "Yes" else "No"),
      "Bad Data" -> (if(telemetryDTO.isBad) "Yes" else "No"),
      "Observable" -> (if(telemetryDTO.isObservable) "Yes" else "No")
    )
  }

  implicit val telemetrySeqWrites = new Writes[Seq[TelemetryDTO]] {
    def writes(ts : Seq[TelemetryDTO]): JsObject = {
      val jos = for(te <- ts) yield te.measType -> Json.toJsFieldJsValueWrapper(te)
      Json.obj(
        jos :_*
      )
    }

  }

  implicit val busWrites = new Writes[BusDTO] {
    def writes(busDTO: BusDTO) : JsValue = Json.obj(
      "id" -> busDTO.id,
      "Name" -> busDTO.name,
      "parent" -> busDTO.parent,
      "Island ID" -> busDTO.island,
      "Control Area" -> busDTO.controlArea,
      "Owner" -> busDTO.owner,
      "Voltage Angle(deg)" -> busDTO.va,
      "Substation" -> busDTO.substation,
      "Voltage Level(KV)" -> busDTO.voltageLevel,
      "pg" -> busDTO.totalPG,
      "Total Q gen(MVAr)" -> busDTO.totalQG,
      "pd" -> -busDTO.totalPD,
      "Total Q demand(MVAr)" -> -busDTO.totalQD,
      "pgdiff" -> busDTO.totalPGDiff,
      "Telemetry" -> busDTO.tlist
    )
  }
  
  implicit val genWrites = new Writes[GenDTO] {
    def writes(genDTO : GenDTO): JsObject = Json.obj(
      "id" -> genDTO.id,
      "name" -> genDTO.name,
      "parent" -> genDTO.parent,
      "mode" -> genDTO.gmode.toString,
      "gtype" -> genDTO.gtype.toString,
      "opMinP" -> genDTO.opMinP,
      "opMaxP" -> genDTO.opMaxP,
      "minQ" -> genDTO.minQ,
      "maxQ" -> genDTO.maxQ,
      "pSet" -> genDTO.pSet,
      "qSet" -> genDTO.qSet,
      "pg" -> genDTO.pg,
      "qg" -> genDTO.qg,
      "pd" -> genDTO.pd,
      "qd" -> genDTO.qd,
      "pgdiff" -> genDTO.pgdiff,
      "isRegKV" -> genDTO.isRegKV,
      "vSet" -> genDTO.vSet,
      "regBus" -> genDTO.regBus,
      "unitInAVR" -> genDTO.unitInAVR,
      "isGenerating" -> genDTO.isGenerating
    )
  }
  
  implicit val loadWrites = new Writes[LoadDTO] {
    def writes(loadDTO: LoadDTO): JsObject = Json.obj(
      "id" -> loadDTO.id,
      "name" -> loadDTO.name,
      "parent"-> loadDTO.parent,
      "maxP" -> loadDTO.maxP,
      "maxQ" -> loadDTO.maxQ,
      "pd" -> loadDTO.pd,
      "qd" -> loadDTO.qd,
      "pg" -> loadDTO.pg,
      "qg" -> loadDTO.qg,
      "pgdiff" -> loadDTO.pgdiff
    )
  }
  
  implicit val branchWrites = new Writes[BranchDTO] {
    def writes(branchDTO: BranchDTO): JsObject = Json.obj(
      "id" -> branchDTO.id,
      "Name" -> branchDTO.name,
      "source" -> branchDTO.source,
      "target" -> branchDTO.target,
      "R" -> branchDTO.r,
      "X" -> branchDTO.x,
      "From Tap" -> branchDTO.fromTap,
      "To Tap" -> branchDTO.toTap,
      "Gmag" -> branchDTO.gmag,
      "Bmag" -> branchDTO.bmag,
      "From Side Bchg" -> branchDTO.fromBchg,
      "To Side Bchg" -> branchDTO.toBchg,
      "Phase Shift" -> branchDTO.phaseShift,
      "LT_Rating" -> branchDTO.ltRating,
      "fromP" -> branchDTO.fromP,
      "toP" -> branchDTO.toP,
      "fromQ" -> branchDTO.fromQ,
      "toQ" -> branchDTO.toQ,
      "flow_MVA" -> branchDTO.flow,
      "Telemetry" -> branchDTO.tlist
    )
  }
  
  implicit val twoTermDevWrites = new Writes[TwoTermDevDTO] {
    def writes(twoTermDevDTO: TwoTermDevDTO): JsObject = Json.obj(
      "id" -> twoTermDevDTO.id,
      "name" -> twoTermDevDTO.name,
      "source" -> twoTermDevDTO.source,
      "target" -> twoTermDevDTO.target,
      "fromP" -> twoTermDevDTO.fromP,
      "toP" -> twoTermDevDTO.toP,
      "fromQ" -> twoTermDevDTO.fromQ,
      "toQ" -> twoTermDevDTO.toQ
    )
  }

  implicit val oneTermDevWrites = new Writes[OneTermDevDTO] {
    def writes(oneTermDevDTO: OneTermDevDTO): JsObject = Json.obj(
      "id" -> oneTermDevDTO.id,
      "name" -> oneTermDevDTO.name,
      "parent" -> oneTermDevDTO.parent,
      "dtype" -> oneTermDevDTO.dtype,
      "pg" -> 0,
      "qg" -> 0,
      "pd" -> 0,
      "qd" -> 0,
      "pgdiff" -> 0
    )
  }
  
  implicit def elementDTOWrites(elementDTO : ElementDTO) : JsValueWrapper = elementDTO match {
    case e : BusDTO => busWrites.writes(e)
    case e : ParentNodeDTO => Json.obj("id" -> e.id, "parent" -> e.parent)
    case e : GenDTO => genWrites.writes(e)
    case e : LoadDTO => loadWrites.writes(e)
    case e : BranchDTO => branchWrites.writes(e)
    case e : TwoTermDevDTO => twoTermDevWrites.writes(e)
    case e : OneTermDevDTO => oneTermDevWrites.writes(e)
    case _ => Json.obj("id" -> "Data Transfer Object Not Found")
  }
  
  implicit val elementWrites = new Writes[Element] {
    def writes(element : Element): JsObject = Json.obj(
      "group" -> element.group,
      "data" -> element.data,
      "position" -> element.position,
      "selectable" -> element.selectable,
      "locked" -> element.locked,
      "grabbable" -> element.grabbable,
      "classes" -> element.classes
    )
  }

  implicit val elementSeqWrites  = new Writes[Seq[Element]] {
    def writes(es : Seq[Element]) = Json.toJson(es)
  }

  implicit val complexWrites = new Writes[ComplexDTO] {
    def writes(complexDTO: ComplexDTO) = Json.obj(
      "real" -> complexDTO.real,
      "imag" -> complexDTO.imag
    )
  }

  implicit val pfResultsWrites = new Writes[PFResultsDTO] {
    def writes(pfrd: PFResultsDTO): JsObject = Json.obj(
      "island" -> pfrd.island,
      "status" -> pfrd.status,
      "statusP" -> pfrd.statusP,
      "statusQ" -> pfrd.statusQ,
      "generatedMW" -> pfrd.generatedMW,
      "generatedMVAr" -> pfrd.generatedMVAr,
      "loadMW" -> pfrd.loadMW,
      "loadMVAr" -> pfrd.loadMVAr,
      "worstVoltageBus" -> pfrd.worstVoltageBus,
      "worstVoltageValue" -> pfrd.worstVoltage,
      "worstPMismatchBus" -> pfrd.worstPMismatchBus,
      "worstPMismatchValue" -> pfrd.worstPMismatchValue,
      "worstQMismatchBus" -> pfrd.worstPMismatchBus,
      "worstQMismatchValue" -> pfrd.worstQMismatchValue,
      "numIterP" -> pfrd.numIterP,
      "numIterQ" -> pfrd.numIterQ,
      "maxVoltageLimit" -> pfrd.maxVoltageLimit,
      "minVoltageLimit" -> pfrd.minVoltageLimit,
      "errorMessage" -> pfrd.errorMessage
    )
  }

  implicit val pfResultsSeqWrites  = new Writes[Seq[PFResultsDTO]] {
    def writes(es : Seq[PFResultsDTO]) = Json.toJson(es)
  }

  implicit val badDataDetectorWrites = new Writes[BadDataDetectorDTO] {
    def writes(badDataDetectorDTO: BadDataDetectorDTO): JsObject = Json.obj(
      "Probability" -> badDataDetectorDTO.probability,
      "Number of States" -> badDataDetectorDTO.nstate,
      "Error" -> badDataDetectorDTO.error,
      "Threshold" -> badDataDetectorDTO.threshold,
      "Degrees of Freedom" -> badDataDetectorDTO.degfree
      //"z0injPower" -> badDataDetectorDTO.z0injPower
    )
  }

  implicit val measRowWrites  = new Writes[MeasRowDTO] {
    def writes(measRowDTO: MeasRowDTO): JsObject = Json.obj(
      //"Index" -> measRowDTO.index,
      "Per_Unit_Measurement" -> measRowDTO.meas,
      "Old_Measurement" -> measRowDTO.oldMeas,
      "Per_Unit_Sigma" -> measRowDTO.sigmaPU,
      "Is_Zero_Injection" -> measRowDTO.isZeroInjection,
      "Residual" -> measRowDTO.residual,
      "Telemetry_Data" -> measRowDTO.telemetryString
    )
  }

  implicit val seResultsWrites = new Writes[SEResultsDTO] {
    def writes(sEResultsDTO: SEResultsDTO): JsObject = Json.obj(
      "iter" -> sEResultsDTO.iteration,
      "norm" -> sEResultsDTO.norm,
      "threshold" -> sEResultsDTO.threshold,
      "error" -> sEResultsDTO.error,
      "degFree" -> sEResultsDTO.degFreedom,
      "obsIsland" -> sEResultsDTO.observableIsland,
      "matrixRowCount" -> sEResultsDTO.matrixRowCounts,
      "obsRowCount" -> sEResultsDTO.observableRowCount,
      "zInjRowCount" -> sEResultsDTO.zeroInectionRows,
      "badMeasRows" -> Json.toJson(sEResultsDTO.badRows)
    )
  }

  implicit val ctgcyDTOWrites = new Writes[CtgcyResultDTO] {
    def writes(ctgcyResultDTO: CtgcyResultDTO): JsObject = Json.obj(
      "Status" -> ctgcyResultDTO.status,
      "Case_Type" -> ctgcyResultDTO.caseType,
      "Contingency_Branch" -> ctgcyResultDTO.contingency,
      "Affected_Branch" -> ctgcyResultDTO.affected,
      "Pre_Cont_Flow" -> ctgcyResultDTO.preContFlow,
      "Post_Cont_Flow" -> ctgcyResultDTO.postContFlow,
      "Overflow_Percent" -> ctgcyResultDTO.overflowPercent,
      "Severity" -> ctgcyResultDTO.severity
    )}

  implicit val ctgcyResultsDTOWrites = new Writes[Seq[CtgcyResultDTO]] {
    def writes(scr : Seq[CtgcyResultDTO]): JsValue = Json.toJson(scr)
  }

  implicit val seResultsSeqWrites = new Writes[Seq[SEResultsDTO]] {
    def writes(sser : Seq[SEResultsDTO]): JsValue = Json.toJson(sser)
  }

}
