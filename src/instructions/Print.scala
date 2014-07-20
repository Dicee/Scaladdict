package instructions

import expressions.Expression

class PrintString(val a : Any) extends Instruction[Unit] {
	def exec(ev : Environment)  : (Unit,Environment) = {
		println(a)
		return ({},ev)
	}
	def format(indent : String) : String = "%sprintln(\"%s\")".format(indent,a)
}

class PrintExpr(val expr : Expression) extends Instruction[Unit] {
	def exec(ev : Environment)  : (Unit,Environment) = {
		println(expr.valuation(ev))
		return ({},ev)
	}
	def format(indent : String) : String = "%sprintln(%s)".format(indent,expr)
}