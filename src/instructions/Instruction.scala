package instructions

import expressions.Expression

trait Instruction[+T] extends Cloneable {
	def exec(ev : Environment) : (T,Environment)
	def formatInstr(indent : String) : String
}

object NOP extends Instruction[Unit] {
	def exec(ev : Environment)  : (Unit,Environment) = ({},ev)
	def formatInstr(indent : String): String = "{ }"
}

abstract class AbstractInstruction[+T] extends Instruction[T] {
	def toBlock : Block   = if (isInstanceOf[Block]) asInstanceOf[Block] else new Block(this)
	override def toString = formatInstr("")
}

abstract class EvaluableInstruction extends Expression with Instruction[Double] {
	def toBlock : Block   = new Block(this)
	override def toString = formatInstr("")
}