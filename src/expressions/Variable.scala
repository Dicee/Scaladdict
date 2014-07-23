package expressions

import instructions.Environment
import instructions.UndeclaredIdentifierException

class Variable(val name : String) extends Expression {
	def valuation(ev : Environment) = ev.get(name)
	def formatExpr                  = name
}

