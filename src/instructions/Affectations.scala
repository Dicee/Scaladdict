package instructions

import expressions.Expression
import expressions.Variable

trait Affectation extends Instruction[Any] { def name : String }

class Assignment(private val varName : String, val rightMember : Expression) extends Instruction[Double] with Affectation {
	def this(variable : Variable, rightMember : Expression) = this(variable.name,rightMember) 
	
	def name = varName
	def exec(ev : Environment): (Double,Environment) = {
		var result : Double = rightMember.valuation(ev)
		ev += varName -> Some(result)
		return (result,ev)
	}
	
	def format(indent : String) = "%s%s = %s;".format(indent,varName,rightMember)
	override def clone          = new Assignment(varName,rightMember)
}

class Declaration(private val varName : String, val rightMember : Option[Expression]) extends Instruction[Unit] with Affectation {
	def this(variable : Variable)                    = this(variable.name,None)
	def this(varName : String )                      = this(varName,None)
	def this(variable : Variable, expr : Expression) = this(variable.name,Some(expr))
	def this(varName : String, expr : Expression)    = this(varName,Some(expr))
	
	def name = varName
	def exec(ev : Environment): (Unit,Environment) = {
		return ({},ev !+= varName -> initValue(ev))
	}
	
	private def initValue(ev : Environment) = {
		rightMember match {
			case Some(expr) => Some(expr.valuation(ev))
			case None       => None
		}
	}
	
	def format(indent : String) = rightMember match {
		case Some(expr) => "%svar %s = %s;".format(indent,varName,expr)
		case None       => "%svar %s;".format(indent,varName)
	} 
	
	override def clone = new Declaration(varName,rightMember)
}
