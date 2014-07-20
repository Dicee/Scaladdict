package instructions

abstract class Instruction[+T] extends Cloneable {
	def exec(ev : Environment)  : (T,Environment)
	def format(indent : String) : String
	def toBlock : Block = if (isInstanceOf[Block]) asInstanceOf[Block] else new Block(this)
	override def toString = format("")
}

object NOP extends Instruction[Unit] {
	def exec(ev : Environment)  : (Unit,Environment) = ({},ev)
	def format(indent : String) : String = "{ }"
}