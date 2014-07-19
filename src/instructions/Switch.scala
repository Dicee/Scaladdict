package instructions

import expressions.Expression
import scala.collection.mutable.HashMap

class Switch(val expr : Expression, val default : Option[Block], keyVals : (Double,Block)*) extends Instruction[Unit] {
	private var cases : HashMap[Double,Block] = new HashMap()
	keyVals.foreach(keyVal => this += keyVal)
	
	def this(expr : Expression, default : Block, keyVals : (Double,Block)*) = {
		this(expr,Some(default))
		keyVals.foreach(keyVal => this += keyVal)
	}
	
	def this(expr : Expression, keyVals : (Double,Block)*) = {
		this(expr,None)
		keyVals.foreach(keyVal => this += keyVal)
	}
	
	private def +=(kv : (Double,Block)) : Unit = {
		if (cases.contains(kv._1 ))
			throw new IllegalArgumentException("Duplicate case " + kv._1)
		else 
			cases += kv
	}
	
	def exec(ev : Environment) : (Unit,Environment) = {
		var newEv : Environment = new Environment
		var instr = cases.get(expr.valuation(ev)) match {
			case Some(instruction) => newEv = instruction.exec(ev)._2
			case None              => 
				default match {
					case Some(instruction) => newEv = instruction.exec(ev)._2
					case None              => newEv = ev
				}
		}
		return ({},newEv)
	}
	
	def format(indent : String) : String = {
		var casesStr : String = 
			if (cases.isEmpty) "" 
			else {
				var sb : StringBuilder = new StringBuilder
				cases.foreach{ case (key,value) => 
					sb.append("\n\t%scase %f : %s".format(indent,key,value.format("",indent + "\t")))
				}
				sb.toString
			}
			
		var defaultStr : String = default match {
			case Some(instruction) => "\n\t%sdefault : %s\n%s}".format(indent,instruction.format("",indent + "\t"),indent)
			case None              => "\n%s}".format(indent)
		}
		return "%sswitch (%s) {".format(indent,expr) + casesStr + defaultStr
	}
	
	override def clone : Switch = { var switch = new Switch(expr,default); switch.cases = cases.clone; return switch; }
}