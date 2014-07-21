package instructions

import predicates.Predicate

class While(val continuation : Predicate, instr : Instruction[Any]*) extends Block(instr : _*) {
	override def exec(ev : Environment) : (Unit,Environment) = {
		var cpEv = ev.clone
		while (continuation.test(cpEv)) instructions.foreach(instr => cpEv = instr.exec(cpEv)._2)
		cpEv.foreach{ case (key,value) => if (ev.contains(key)) ev += key -> value }
		return ({},ev)
	}
	override def formatInstr(indent : String) = "while (%s) %s".format(continuation,super.format("",indent))
	override def clone = new While(continuation,instructions.clone.toSeq : _*)
}