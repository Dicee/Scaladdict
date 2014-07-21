package instructions

import predicates.Predicate

class While(val continuation : Predicate, instr : Instruction[Any]*) extends Block(instr : _*) {
	override def exec(ev : Environment) = {
		var cpEv = ev.clone
		while (continuation.test(cpEv)) instructions.foreach(instr => instr.exec(cpEv))
		cpEv.foreach{ case (key,value) => if (ev.contains(key)) ev += key -> value }
	}
	override def formatInstr(indent : String) = "while (%s) %s".format(continuation,super.format("",indent))
	override def clone = new While(continuation,instructions.clone.toSeq : _*)
}