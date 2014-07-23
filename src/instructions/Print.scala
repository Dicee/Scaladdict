package instructions

import expressions.Expression

trait Print extends Instruction[Unit] {
	private[instructions] def containsReturn = false
}

class PrintString(val a : Any) extends Print {
	def exec(ev : Environment)       = println(a)
	def formatInstr(indent : String) = "%sprintln(\"%s\");".format(indent,a.toString)
}

class PrintExpr(val expr : Expression) extends Print {
	def exec(ev : Environment)       = println(expr.valuation(ev))
	def formatInstr(indent : String) = "%sprintln(%s);".format(indent,expr)
}