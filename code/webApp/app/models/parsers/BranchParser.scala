package models.parsers

import com.powerdata.openpa.core.{ACBranch, Line}
import com.powerdata.openpa.core.TelemetryList.MeasType
import com.powerdata.openpa.tools.PAMath

object BranchParser extends OpenPaParser[ACBranch] {
  override def fprops =
    ("fromp" | "pfrom" | "fp" | "pf" | "fromq" | "qfrom" | "fq" | "qf" | "X" | "R" | "flow" | "ltrating") ^^ {t => FLOATFUNCTION(t)}

  override def sprops =
    ("id" | "frombus" | "tobus" | "name" | "index") ^^ {t => STRINGFUNCTION(t)}
  
  override def bprops = 
    ("fromp_telem" | "top_telem" | "fromq_telem" | "toq_telem"
  | "fromp_bdd" | "top_bdd" | "fromq_bdd" | "toq_bdd") ^^ {t => BOOLEANFUNCTION(t)}

  override def evalf(floatOperand: FLOATOPERAND)(implicit br:ACBranch) = floatOperand match {
    case f:FLOATNUMBER => f.v
    case f:FLOATFUNCTION => f.v match {
      case "fromp" | "fp" | "pf" | "pfrom" => br.getFromP
      case "fromq" | "qfrom" |  "qf" | "fq" => br.getFromQ
      case "X" => br.getX
      case "R" => br.getR
      case "flow" => math.max(PAMath.calcMVA(br.getFromP,br.getFromQ), PAMath.calcMVA(br.getToP, br.getToQ))
      case "ltrating" => br.getLTRating
    }
  }

  override def evals(stringOperand: STRINGOPERAND)(implicit br:ACBranch) = stringOperand match {
    case f:STRINGLITERAL => f.v
    case f:STRINGFUNCTION => f.v match {
      case "id" => br.getID
      case "frombus" => br.getFromBus.getID
      case "tobus" => br.getToBus.getID
      case "name" => br.getName
      case "index" => br.getIndex.toString
    }
  }

  override def evalb(booleanOperand : BOOLEANOPERAND)(implicit br: ACBranch) = booleanOperand match {
    case b : BOOLEANVALUE => b.v
    case b : BOOLEANFUNCTION => b.v match {
      case "fromp_telem" => br.getTelemetry.get(MeasType.BranchFromP).isTelemetered
      case "top_telem" => br.getTelemetry.get(MeasType.BranchToP).isTelemetered
      case "fromq_telem" => br.getTelemetry.get(MeasType.BranchFromQ).isTelemetered
      case "toq_telem" => br.getTelemetry.get(MeasType.BranchToQ).isTelemetered

      case "fromp_bdd" => br.getTelemetry.get(MeasType.BranchFromP).isBadData
      case "top_bdd" => br.getTelemetry.get(MeasType.BranchToP).isBadData
      case "fromq_bdd" => br.getTelemetry.get(MeasType.BranchFromQ).isBadData
      case "toq_bdd" => br.getTelemetry.get(MeasType.BranchToQ).isBadData
      
    }
  }
}
