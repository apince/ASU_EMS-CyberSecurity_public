package models.parsers

import com.powerdata.openpa.core.{Bus, TelemetryList}
import com.powerdata.openpa.core.TelemetryList.MeasType

object BusParser extends OpenPaParser[Bus] {
  override def fprops =
    ("voltagelevel" | "pinj" | "qinj" | "vmpu"| "vm" | "va" | "active_load" | "active_gen" ) ^^ {t => FLOATFUNCTION(t)}

  override def sprops =
    ("id" | "station" | "name" | "owner" | "area" | "index" | "genid") ^^ {t => STRINGFUNCTION(t)}

  override def bprops = ("pinj_telem" | "qinj_telem" | "pinj_bdd" | "qinj_bdd") ^^ {t => BOOLEANFUNCTION(t)}

  override def evalf(floatOperand: FLOATOPERAND)(implicit bu:Bus) = floatOperand match {
      case f:FLOATNUMBER => f.v
      case f:FLOATFUNCTION => f.v match {
        case "voltagelevel" => bu.getVoltageLevel.getBaseKV
        case "pinj" => bu.getTelemetry.get(TelemetryList.MeasType.BusInjP).getMeasurement
        case "qinj" => bu.getTelemetry.get(TelemetryList.MeasType.BusInjQ).getMeasurement
        case "vmpu" => bu.getVM / bu.getVoltageLevel.getBaseKV
        case "vm" => bu.getTelemetry.get(TelemetryList.MeasType.BusVM).getMeasurement
        case "va" => bu.getVA
        case "active_load" => bu.getLoads.getP.sum
        case "active_gen" => bu.getGenerators.getP.sum
      }
    }

  override def evals(stringOperand: STRINGOPERAND)(implicit bu:Bus) = stringOperand match {
    case f:STRINGLITERAL => f.v
    case f:STRINGFUNCTION => f.v match {
      case "id" => bu.getID
      case "station" => bu.getStation.getID
      case "name" => bu.getName
      case "owner" => bu.getOwner.getID
      case "area" => bu.getArea.getID
      case "index" => bu.getIndex.toString
      case "genid" => bu.getGenerators.getID.mkString
    }
  }

  override def evalb(booleanOperand : BOOLEANOPERAND)(implicit bu: Bus) = booleanOperand match {
    case b : BOOLEANVALUE => b.v
    case b : BOOLEANFUNCTION => b.v match {
      case "pinj_telem" => bu.getTelemetry.get(MeasType.BusInjP).isTelemetered
      case "qinj_telem" => bu.getTelemetry.get(MeasType.BusInjQ).isTelemetered
      case "pinj_bdd" => bu.getTelemetry.get(MeasType.BusInjP).isBadData
      case "qinj_bdd" => bu.getTelemetry.get(MeasType.BusInjQ).isBadData
    }
  }
}
