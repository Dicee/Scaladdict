package expressions

import instructions.Environment

class Value(val value : Double) extends Expression {
	def valuation(ev : Environment) = value
	def formatExpr                  = value.toString
}