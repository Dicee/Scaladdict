package instructions.affectations

import instructions.EvaluableInstruction
import instructions.Environment

private[affectations] abstract class IncDec(private val varName : String, val after : Boolean, val inc : Double) 
	extends EvaluableInstruction with Affectation {
	
	def this(varName : String, inc : Double) = this(varName,true,inc)
	def name = varName
	def exec(ev : Environment): (Double,Environment) = {
		var x = ev.get(varName) + inc
		ev   += varName -> Some(x)
		return (x,ev)
	}
	
	def valuation(ev : Environment) : Double = {
		var x = ev.get(varName)
		var y = exec(ev)._1 
		return if (after) x else y
	}
}

class Increment(varName : String, incAfter : Boolean) extends IncDec(varName,incAfter,1) {
	def formatInstr(indent : String) = (if (incAfter) "%s%s++" else "++%s%s").format(indent,varName)
	def formatExpr                   = formatInstr("")
	override def clone               = new Increment(varName,incAfter)
}

class Decrement(varName : String, incAfter : Boolean) extends IncDec(varName,incAfter,-1) {
	def formatInstr(indent : String) = (if (incAfter) "%s%s--" else "--%s%s").format(indent,varName)
	def formatExpr                   = formatInstr("")
	override def clone               = new Decrement(varName,incAfter)
}