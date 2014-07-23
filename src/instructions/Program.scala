package instructions

class Program(val block : Block, val functions : FunctionDef*) {
	override def toString = functions.addString(new StringBuilder,"","\n\n","\n\n").toString + "void main() " + block.formatInstr("")
	def exec : Unit = {
		var ev : Environment = new Environment
		functions.foreach(fun => ev.defFunction(fun))
		block.instructions.foreach(instr => { instr.exec(ev) /*; println("Environment = " + ev)*/ })
	} 
}