package instructions

class Program(val block : Block) extends Iterable[Instruction[Any]] {
	def iterator          = block.iterator
	override def toString = "Program " + block.format("")
	def exec : Unit = {
		var ev : Environment = new Environment
		block.instructions.foreach(instr => { ev = instr.exec(ev)._2 ; println("Environment = " + ev) })
	} 
}