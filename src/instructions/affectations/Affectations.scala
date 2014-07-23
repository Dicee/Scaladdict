package instructions.affectations

import expressions.Expression
import expressions.Variable
import instructions._

trait Affectation extends Instruction[Any] { def name : String }

class Assignment(protected val varName : String, val rightMember : Expression) extends EvaluableInstruction with Affectation {
	def this(variable : Variable, rightMember : Expression) = this(variable.name,rightMember) 
	
	def name = varName
	def exec(ev : Environment) : Double = {
		var result = rightMember.valuation(ev)
		ev        += varName -> Some(result)
		return result
	}
	def valuation(ev : Environment)  = exec(ev)
	def formatExpr                   = "(%s = %s)".format(varName,rightMember)
	def formatInstr(indent : String) = "%s%s = %s".format(indent,varName,rightMember)
	override def clone               = new Assignment(varName,rightMember)
}

class Declaration(private val varName : String, val rightMember : Option[Expression]) extends Instruction[Unit] with Affectation {
	def this(variable : Variable)                    = this(variable.name,None)
	def this(varName : String )                      = this(varName,None)
	def this(variable : Variable, expr : Expression) = this(variable.name,Some(expr))
	def this(varName : String, expr : Expression)    = this(varName,Some(expr))
	
	def name = varName
	private[instructions] def containsReturn = false
	def exec(ev : Environment) = ev !+= varName -> initValue(ev)

	private def initValue(ev : Environment) = {
		rightMember match {
			case Some(expr) => Some(expr.valuation(ev))
			case None       => None
		}
	}
	
	def formatInstr(indent : String) = rightMember match {
		case Some(expr) => "%svar %s = %s".format(indent,varName,expr)
		case None       => "%svar %s".format(indent,varName)
	} 
	override def clone = new Declaration(varName,rightMember)
}

private[affectations] class OpAssignment(varName : String, expr : Expression, symbol : String, op : (Double,Double) => Double) 
	extends Assignment(varName,expr) with Affectation {
	
	override def exec(ev : Environment) : Double= {
		var eval  = rightMember.valuation(ev)
		var value = op(ev.get(varName),eval)
		ev       += varName -> Some(value)
		return value
	}
	override def formatExpr                   = "(%s %s= %s)".format(varName,symbol,rightMember)
	override def formatInstr(indent : String) = "%s%s %s= %s".format(indent,varName,symbol,rightMember)
	override def clone                        = new OpAssignment(varName,expr,symbol,op)
}

class PlusAssignment (varName : String, expr : Expression) extends OpAssignment(varName,expr,"+",(a,b) => a + b) 
class MinusAssignment(varName : String, expr : Expression) extends OpAssignment(varName,expr,"-",(a,b) => a - b) 
class ProdAssignment (varName : String, expr : Expression) extends OpAssignment(varName,expr,"*",(a,b) => a * b) 
class DivAssignment  (varName : String, expr : Expression) extends OpAssignment(varName,expr,"/",(a,b) => a / b) 