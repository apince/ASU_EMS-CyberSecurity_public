package models.parsers

import java.text.ParseException

import com.powerdata.openpa.core.AbstractBaseObject

import scala.util.parsing.combinator.JavaTokenParsers

abstract class OpenPaParser[T <: AbstractBaseObject] extends JavaTokenParsers with OpenPaAST {

  override def skipWhitespace: Boolean = true
  //Subclasses should define specific tokens they need for parsing
  def fprops: Parser[FLOATFUNCTION]
  def sprops: Parser[STRINGFUNCTION]
  def bprops: Parser[BOOLEANFUNCTION]

  //Common among all OpenPaParsers
  def floatNum : Parser[FLOATNUMBER] =
    floatingPointNumber ^^ {t => FLOATNUMBER(t.toFloat)}

  def sident : Parser[STRINGLITERAL] =
    ("'" ~> """[^']+""".r <~ "'") ^^ {t => STRINGLITERAL(t)}

  def bool : Parser[BOOLEANVALUE] = ("true" | "false") ^^ {t => BOOLEANVALUE( t match {
    case "true" => true
    case "false" => false
  })
  }

  def compf : Parser[BOOLEANOPERAND] =
    fprops ~ (">=" | "<=" | "==" | "!=" | "<" | ">") ~ (floatNum | fprops) ^^ {t => t._1._2 match {
      case ">=" => GTEQ(t._1._1, t._2)
      case ">" => GT(t._1._1, t._2)
      case "<=" => LTEQ(t._1._1, t._2)
      case "<" => LT(t._1._1, t._2)
      case "==" => EQF(t._1._1, t._2)
      case "!=" => NEQF(t._1._1, t._2)
    }}

  def comps : Parser[BOOLEANOPERAND] =
    sprops ~ ("eq" | "contains") ~ sident ^^ {t => t._1._2 match {
      case "eq" => EQS(t._1._1,t._2)
      case "contains" => CONTAINS(t._1._1,t._2)
    }}

  def compb : Parser[BOOLEANOPERAND] =
    bprops ~ "is" ~ bool ^^ {t => XNOR(t._1._1, t._2)}

  def expr  : Parser[OPERAND] = chainl1(comps|compf|compb, "and" ^^^ AND | "or" ^^^ OR)

  //Interpreting the AST. evalf and evals need to be implemented by subclass
  //since they are different for each parser
  def evalf(floatOperand: FLOATOPERAND)(implicit ev:T):Float

  def evals(stringOperand: STRINGOPERAND)(implicit ev:T):String

  def evalb(booleanOperand : BOOLEANOPERAND)(implicit ev : T):Boolean

  //eval is shared among all parser subclasses
  def eval(r: OPERAND)(implicit ev:T):Boolean = r match {
    case c:AND => eval(c.op1) && eval(c.op2)
    case c:OR => eval(c.op1) || eval(c.op2)
    case c:XNOR => !(evalb(c.op1) ^ evalb(c.op2))

    case c:GT => evalf(c.fop1).compareTo(evalf(c.fop2)) > 0
    case c:GTEQ => evalf(c.fop1).compareTo(evalf(c.fop2)) >= 0
    case c:LT => evalf(c.fop1).compareTo(evalf(c.fop2)) < 0
    case c:LTEQ => evalf(c.fop1).compareTo(evalf(c.fop2)) <= 0
    case c:EQF => evalf(c.fop1).compareTo(evalf(c.fop2)) == 0
    case c:NEQF => evalf(c.fop1).compareTo(evalf(c.fop2)) != 0

    case c:EQS => evals(c.sop1).toLowerCase == evals(c.sop2).toLowerCase
    case c:CONTAINS => evals(c.sop1).toLowerCase.contains(evals(c.sop2).toLowerCase)

    case _ => throw new ParseException("Parsing failed", 0)
  }
}
