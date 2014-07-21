package expressions

import instructions.Environment
import predicates._

abstract class Expression extends Predicate {
	def valuation(ev : Environment) : Double 
	def test(ev : Environment) : Boolean = valuation(ev) != 0
	
	def + (expr : Expression) = new Plus(this,expr)
	def - (expr : Expression) = new Minus(this,expr)
	def * (expr : Expression) = new Dot(this,expr)
	def / (expr : Expression) = new Div(this,expr)
	def ==(expr : Expression) = new Equal(this,expr)
	def > (expr : Expression) = new GreaterThan(this,expr)
	def >=(expr : Expression) = new GreaterThan(this,expr,true)
	def < (expr : Expression) = new LowerThan(this,expr)
	def <=(expr : Expression) = new LowerThan(this,expr,true)
	def opposite              = new Opposite(this)
	
	def formatExpr : String
	override def toString = formatExpr
}