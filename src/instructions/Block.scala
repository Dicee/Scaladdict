package instructions

import instructions.affectations.Declaration

class Block(instr : Instruction[Any]*) extends AbstractInstruction[Unit] {
	val instructions : Array[Instruction[Any]] = instr.toArray	
	
	def iterator : Iterator[Instruction[Any]] = instructions.iterator
	def exec(ev : Environment): (Unit,Environment) = {
		var cpEv = ev.clone
		instructions.foreach(instr => cpEv = instr.exec(cpEv)._2)
		cpEv.foreach{ case (key,value) => if (ev.contains(key)) ev += key -> value }
		return ({},ev)
	}
	
	def formatInstr(indent : String): String =  format(indent,indent)
	def format(indentBefore : String, indent : String) : String = {
		if (instructions.isEmpty)
			return indentBefore + "{ }"
			
		var result : String = {			
			var sb : StringBuilder = new StringBuilder
			instructions.foreach{ instr =>
				sb.append(instr.formatInstr(indent + "\t") + "\n")
			}
			sb.toString
		}
		var showBrackets = instructions.size != 1 || instructions(0).isInstanceOf[Block] || instructions(0).isInstanceOf[Declaration] 
		return if (showBrackets) "%s{\n%s%s}".format(indentBefore,result,indent) else indentBefore + result.trim
	}
	
	override def clone : Block = new Block(instructions.clone.toSeq : _*)
	def size = instructions .size
}