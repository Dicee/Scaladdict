package instructions

import predicates.Predicate

class For(val continuation : Predicate, instr : Instruction[Any]*) extends Block(instr : _*) {
	private var initialization : Array[Affectation] = Array()
	private var update         : Array[Instruction[Any]] = Array()
	
	def addUpdateClause(instructions : Instruction[Any]*) = update = update ++ instructions
	def addInitClause(affectations : Affectation*) {
		for (affectation <- affectations) {
			if (affectation.isInstanceOf[Declaration]) {
				var d = affectation.asInstanceOf[Declaration]
					d.rightMember match {
					case Some(expr) => { }
					case None       => throw new IllegalArgumentException("Variable %s must be initialized".format(affectation.name))
				}
			}
			initialization :+ affectation
		}
	}
	
	override def exec(ev : Environment) : (Unit,Environment) = {
		var cpEv = ev.clone
		for (affectation <- initialization) cpEv = affectation.exec(ev)._2
		instructions.foreach(instr => {
			cpEv = instr.exec(cpEv)._2
			for (updateClause <- update) cpEv = updateClause.exec(cpEv)._2
		})
		cpEv.foreach{ case (key,value) => if (ev.contains(key)) ev += key -> value }
		return ({},ev)
	}
	
	override def format(indent : String) : String = {
		var initStr     = initialization.addString(new StringBuilder,", ").toString
		var continueStr = continuation.toString
		var updateStr   = update.addString(new StringBuilder,", ").toString
		return "for (%s ; %s ; %s) %s".format(initStr,continueStr,updateStr,super.format("",indent))
	}
	
	override def clone : For = { 
		var res            = new For(continuation)
		res.instructions   = instructions.clone
		res.initialization = initialization.clone
		res.update         = update.clone
		return res
	}
}