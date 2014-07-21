package instructions

import expressions.Expression

class PrintString(val a : Any) extends AbstractInstruction[Unit] {
	def exec(ev : Environment)       = println(a)
	def formatInstr(indent : String) = "%sprintln(\"%s\");".format(indent,a)
}

class PrintExpr(val expr : Expression) extends Instruction[Unit] {
	def exec(ev : Environment)       = println(expr.valuation(ev))
	def formatInstr(indent : String) = "%sprintln(%s);".format(indent,expr)
}