package instructions

class Program(val block : Block) extends Iterable[Instruction[Any]] {
	def iterator          = block.iterator
	override def toString = "Program " + block.formatInstr("")
	def exec : Unit = {
		var ev : Environment = new Environment
		block.instructions.foreach(instr => { instr.exec(ev) ; println("Environment = " + ev) })
	} 
}