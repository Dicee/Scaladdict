package expressions

import instructions.Environment

class Value(val value : Double) extends Expression {
	def valuation(ev : Environment) : Double = value
	override def toString : String = value.toString
}