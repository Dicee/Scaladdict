package instructions

import expressions.Expression
import scala.collection.mutable.HashMap
import predicates.Predicate

class If(val elseClause : Option[Block], keyVals : (Predicate,Block)*) extends Instruction[Unit] {
	private var cases : Array[(Predicate,Block)] = Array()
	setCases(keyVals.toArray)
	
	def this(elseClause : Block, keyVals : (Predicate,Block)*) = {
		this(Some(elseClause),keyVals : _*)
	}
	
	def this(keyVals : (Predicate,Block)*) = {
		this(None,keyVals : _*)
	}
	
	private def setCases(cases : Array[(Predicate,Block)])= {
		if (cases.size < 1) throw new IllegalArgumentException("If clause requires at least one instruction")
		this.cases = cases
	}
	
	def exec(ev : Environment) = {
		for (caseClause <- cases)
			if (caseClause._1 .test(ev))
				caseClause._2 .exec(ev)
		elseClause match {
			case Some(instr) => instr.exec(ev)
			case None        => 
		}
	}
	
	def formatInstr(indent : String): String = {
		var casesStr : String = {
			var sb = new StringBuilder
			var i  = 0
			cases.foreach{ case (predicate,block) => 
				var keyword = if (i == 0) "if" else "\nelse if"
				sb.append("%s%s (%s) %s".format(indent,keyword,predicate,block.format("",indent)))
				i += 1
			}
			sb.toString
		}
			
		var elseStr : String = elseClause match {
			case Some(instruction) => "\n%selse %s".format(indent,instruction.format("",indent))
			case None              => ""
		}
		return casesStr + elseStr
	}
	
	override def clone : If = { var res = new If(elseClause); res.cases = cases.clone; return res; }
}