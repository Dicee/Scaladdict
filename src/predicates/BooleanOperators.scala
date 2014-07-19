package predicates

import instructions.Environment

private[predicates]
		abstract class BooleanBinaryOp(protected val op1 : Predicate, protected val op2 : Predicate, val symbol : String) extends Predicate {
	def test(ev : Environment) : Boolean 
	override def toString : String = String.format("(%s %s %s)",op1,symbol,op2)
}

private[predicates] abstract class BooleanUnaryOp(val op : Predicate, val symbol : String) extends Predicate {
	def test(ev : Environment) : Boolean
	override def toString : String = String.format("%s%s",symbol,op)
}

class And(op1 : Predicate, op2 : Predicate) extends BooleanBinaryOp(op1,op2,"&&") {
	def test(ev : Environment) : Boolean = op1.test(ev) && op2.test(ev)
}

class Not(op : Predicate) extends BooleanUnaryOp(op,"!") {
	def test(ev : Environment) : Boolean = !op.test(ev)
}

class Or(op1 : Predicate, op2 : Predicate) extends BooleanBinaryOp(op1,op2,"||") {
	def test(ev : Environment) : Boolean = op1.test(ev) || op2.test(ev)
}

