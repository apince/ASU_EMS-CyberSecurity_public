package models.parsers

trait OpenPaAST {

  sealed trait OPERAND

  sealed trait FLOATOPERAND extends OPERAND

  sealed trait STRINGOPERAND extends OPERAND

  sealed trait BOOLEANOPERAND extends OPERAND

  case class AND(op1: BOOLEANOPERAND, op2: BOOLEANOPERAND) extends BOOLEANOPERAND

  case class OR(op1: BOOLEANOPERAND, op2: BOOLEANOPERAND) extends BOOLEANOPERAND

  case class XNOR(op1 : BOOLEANOPERAND, op2 : BOOLEANOPERAND) extends BOOLEANOPERAND

  case class BOOLEANFUNCTION(v : String) extends BOOLEANOPERAND

  case class BOOLEANVALUE(v : Boolean) extends BOOLEANOPERAND

  case class FLOATFUNCTION(v: String) extends FLOATOPERAND

  case class FLOATNUMBER(v: Float) extends FLOATOPERAND

  case class STRINGFUNCTION(v: String) extends STRINGOPERAND

  case class STRINGLITERAL(v: String) extends STRINGOPERAND

  case class GTEQ(fop1: FLOATOPERAND, fop2: FLOATOPERAND) extends BOOLEANOPERAND

  case class GT(fop1: FLOATOPERAND, fop2: FLOATOPERAND) extends BOOLEANOPERAND

  case class LTEQ(fop1: FLOATOPERAND, fop2: FLOATOPERAND) extends BOOLEANOPERAND

  case class LT(fop1: FLOATOPERAND, fop2: FLOATOPERAND) extends BOOLEANOPERAND

  case class EQF(fop1: FLOATOPERAND, fop2: FLOATOPERAND) extends BOOLEANOPERAND

  case class NEQF(fop1 : FLOATOPERAND, fop2 : FLOATOPERAND) extends BOOLEANOPERAND

  case class EQS(sop1: STRINGOPERAND, sop2: STRINGOPERAND) extends BOOLEANOPERAND

  case class CONTAINS(sop1: STRINGOPERAND, sop2: STRINGOPERAND) extends BOOLEANOPERAND

}