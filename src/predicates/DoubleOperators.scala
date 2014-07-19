package predicates

import instructions.Environment
import expressions.Expression

private[predicates]
		abstract class BinaryOp(protected val op1 : Expression, protected val op2 : Expression, val symbol : String) extends Predicate {
	override def toString : String = String.format("(%s %s %s)",op1,symbol,op2)
}

private[predicates] abstract class UnaryOp(val op : Expression, val symbol : String) extends Predicate {
	override def toString : String = String.format("%s %s",symbol,op)
}

class GreaterThan(op1 : Expression, op2 : Expression, val equal : Boolean) extends BinaryOp(op1,op2,if (equal) ">=" else ">") {
	def this(op1 : Expression, op2 : Expression) = this(op1,op2,false)
	def test(ev : Environment) : Boolean = {
		var d1 = op1.valuation(ev)
		var d2 = op2.valuation(ev)
		return d1 > d2 || (equal && d1 == d2)
	}
}

class LowerThan(op1 : Expression, op2 : Expression, val equal : Boolean) extends BinaryOp(op1,op2,if (equal) "<=" else "<") {
	def this(op1 : Expression, op2 : Expression) = this(op1,op2,false)
	def test(ev : Environment) : Boolean = {
		var d1 = op1.valuation(ev)
		var d2 = op2.valuation(ev)
		return d1 < d2 || (equal && d1 == d2)
	}
}

class Equal(op1 : Expression, op2 : Expression) extends BinaryOp(op1,op2,"==") {
	def test(ev : Environment) : Boolean = op1.valuation(ev) == op2.valuation(ev)
}