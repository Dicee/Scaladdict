package instructions

import expressions.Expression

trait Instruction[+T] extends Cloneable {
	def exec(ev : Environment) : T
	def formatInstr(indent : String) : String
	def formatInstr : String = formatInstr("")
	override def toString    = formatInstr
	def toBlock : Block   = if (isInstanceOf[Block]) asInstanceOf[Block] else new Block(this)
	private[instructions] def containsReturn : Boolean
}

object NOP extends Instruction[Unit] {
	def exec(ev : Environment)  : Unit = { }
	def formatInstr(indent : String) : String = "{ }"
	private[instructions] def containsReturn = false
}

abstract class EvaluableInstruction extends Expression with Instruction[Double] {
	private[instructions] def containsReturn = false
}
