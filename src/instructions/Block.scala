package instructions

class Block(instr : Instruction[Any]*) extends Instruction[Unit] {
	private[instructions] var instructions : Array[Instruction[Any]] = instr.toArray	
	
	def iterator : Iterator[Instruction[Any]] = instructions.iterator
	def exec(ev : Environment): (Unit,Environment) = {
		var cpEv = ev.clone
		instructions.foreach(instr => cpEv = instr.exec(cpEv)._2)
		cpEv.foreach{ case (key,value) => if (ev.contains(key)) ev += key -> value }
		return ({},ev)
	}
	
	def format(indent : String) : String =  format(indent,indent)
	def format(indentBefore : String, indent : String) : String = {
		if (instructions.isEmpty)
			return indentBefore + "{ }"
			
		var result : String = {			
			var sb : StringBuilder = new StringBuilder
			instructions.foreach{ instr =>
				sb.append(instr.format(indent + "\t") + "\n")
			}
			sb.toString
		}
		var showBrackets = instructions.size != 1 || instructions(0).isInstanceOf[Block] || instructions(0).isInstanceOf[Declaration] 
		return if (showBrackets) "%s{\n%s%s}".format(indentBefore,result,indent) else indentBefore + result.trim
	}
	
	override def clone : Block = { var block = new Block(); block.instructions = instructions.clone; return block; }
	def size = instructions .size
}