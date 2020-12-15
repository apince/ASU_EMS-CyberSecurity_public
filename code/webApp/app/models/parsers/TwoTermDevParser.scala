package models.parsers

import com.powerdata.openpa.core.{ACBranch, TwoTermDev}
import com.powerdata.openpa.core.TelemetryList.MeasType

object TwoTermDevParser extends OpenPaParser[TwoTermDev] {
  override def fprops =
    ("fromp" | "pfrom" | "fp" | "pf" | "fromq" | "qfrom" | "fq" | "qf") ^^ {t => FLOATFUNCTION(t)}

  override def sprops =
    ("id" | "frombus" | "tobus" | "name" | "index") ^^ {t => STRINGFUNCTION(t)}

  override def bprops =
    "inservice" ^^ {t => BOOLEANFUNCTION(t)}

  override def evalf(floatOperand: FLOATOPERAND)(implicit tt:TwoTermDev) = floatOperand match {
    case f:FLOATNUMBER => f.v
    case f:FLOATFUNCTION => f.v match {
      case "fromp" | "fp" | "pf" | "pfrom" => tt.getFromP
      case "fromq" | "qfrom" |  "qf" | "fq" => tt.getFromQ
    }
  }

  override def evals(stringOperand: STRINGOPERAND)(implicit tt:TwoTermDev) = stringOperand match {
    case f:STRINGLITERAL => f.v
    case f:STRINGFUNCTION => f.v match {
      case "id" => tt.getID
      case "frombus" => tt.getFromBus.getID
      case "tobus" => tt.getToBus.getID
      case "name" => tt.getName
      case "index" => tt.getIndex.toString
    }
  }

  override def evalb(booleanOperand : BOOLEANOPERAND)(implicit tt: TwoTermDev) = booleanOperand match {
    case b : BOOLEANVALUE => b.v
    case b : BOOLEANFUNCTION => b.v match {
      case "inservice" => tt.isInService
    }
  }
}
