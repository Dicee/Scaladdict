package expressions

import instructions.Environment
import predicates.Predicate

private[expressions] abstract class BinaryOp(protected val op1 : Expression, protected val op2 : Expression, 
		f : (Double,Double) => Double, symbol : String) extends Expression {
	def formatExpr ="(%s %s %s)".format(op1.formatExpr,symbol,op2.formatExpr)
	def valuation(ev : Environment) : Double = f(op1.valuation(ev),op2.valuation(ev))
}

private[expressions] abstract class UnaryOp(val op : Expression, 
		f : Double => Double, symbol : String) extends Expression {
	def formatExpr = "%s%s".format(symbol,op.formatExpr)
	def valuation(ev : Environment) : Double = f(op.valuation(ev))
}

class Div  (op1 : Expression, op2 : Expression) extends BinaryOp(op1,op2,(x,y) => x / y,"/")
class Minus(op1 : Expression, op2 : Expression) extends BinaryOp(op1,op2,(x,y) => x - y,"-")
class Plus (op1 : Expression, op2 : Expression) extends BinaryOp(op1,op2,(x,y) => x + y,"+")
class Dot  (op1 : Expression, op2 : Expression) extends BinaryOp(op1,op2,(x,y) => x * y,"*")

class Opposite(op : Expression) extends UnaryOp(op,-_,"-")
class IntCast (op : Expression) extends UnaryOp(op,_.toInt,"(int)")

class Ternary(val guard : Predicate, val expr1 : Expression, val expr2 : Expression) extends Expression {
	def valuation(ev : Environment) : Double = if (guard.test(ev)) expr1.valuation(ev) else expr2.valuation(ev)
	def formatExpr = "(%s ? %s : %s)".format(guard,expr1.formatExpr,expr2.formatExpr)
}
