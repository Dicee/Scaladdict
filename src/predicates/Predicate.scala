package predicates

import instructions.Environment

abstract class Predicate {
	def test(ev : Environment) : Boolean 
	def &&(predicate : Predicate) = new And(this,predicate)
	def ||(predicate : Predicate) = new Or(this,predicate)
	def not                       = new Not(this)
}

object True extends Predicate {
	def test(ev : Environment) : Boolean = true
	override def toString                = "true"
}

object False extends Predicate {
	def test(ev : Environment) : Boolean = false
	override def toString                = "false"
}