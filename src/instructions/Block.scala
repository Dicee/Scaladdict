package instructions

import instructions.affectations.Declaration
import expressions._
import predicates._
import instructions.affectations.Assignment

class Block(instr : Instruction[Any]*) extends Instruction[Unit] {
	val instructions : Array[Instruction[Any]] = instr.toArray	
	
	private[instructions] def containsReturn = instructions.find(instr => instr.containsReturn) != None
	def exec(ev : Environment) : Unit = {
		var cpEv = ev.clone
		instructions.foreach(instr => instr.exec(cpEv))
		cpEv.foreach{ case (key,value) => if (ev.contains(key)) ev += key -> value }
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