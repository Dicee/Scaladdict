package expressions

import instructions.Environment
import instructions.UndefinedVariableException

class Variable(val name : String) extends Expression {
	def valuation(ev : Environment) = ev.value(name)
	override def toString : String  = name
}

