package instructions

import scala.collection.mutable.HashSet
import expressions.Expression
import expressions.Value
import expressions.Variable
import predicates.Predicate
import scala.util.control.Breaks._
import predicates.Equal

class FunctionDef(val ident : String, val body : Block, val args : Array[String]) extends Instruction[Unit] {
	/////// That's part of the main constructor ///////
	checkArgs
	private def checkArgs = {
		var argsSet = HashSet[String]() 
		for (arg <- args) if (!argsSet.add(arg)) throw new DuplicateParameterException
		if (!body.containsReturn) throw new MissingReturnException(ident)
	}
	
	private[instructions] def containsReturn = true
	private def findReturn(instr : Instruction[Any]) = {
		var block = instr.toBlock
		block.instructions.find(instr => instr.isInstanceOf[Return]) match { 
			case None    => throw new MissingReturnException(ident) 
			case Some(x) =>
		}
	}
	///////////////////////////////////////////////////
	
	def exec(ev : Environment) = ev.defFunction(this)
	
	def formatInstr(indent : String) : String = {
		var argsStr  = args.addString(new StringBuilder,", ").toString
		var blockStr = {			
			var sb : StringBuilder = new StringBuilder
			body.instructions.foreach{ instr =>	sb.append(instr.formatInstr(indent + "\t") + "\n") }
			sb.toString
		}
		return "%sfunction %s(%s) {\n%s%s}".format(indent,ident,argsStr,blockStr,indent)
	}
	
	override def equals(a : Any) : Boolean = {
		if (!a.isInstanceOf[FunctionDef]) return false
		var other = a.asInstanceOf[FunctionDef]
		return other.ident == ident && other.args.length == args.length
	}
	
	override def hashCode : Int = {
		val prime  = 31
		var result = 1
		result     = prime * result + ident.hashCode
		result     = prime * result + args.length
		return result
	}
}

class FunctionCall(ident: String, args: Array[Expression]) extends EvaluableInstruction {
	def this(ident : String)        = this(ident,Array())
	def valuation(ev : Environment) = exec(ev)
	
	def exec(ev: Environment) : Double = {
		println(args.toSeq.map(expr => expr.valuation(ev)) + "   " + ev)
		var funDef = ev.getDef(ident,args.length)
		var result = 0d
		
		//We first create a local environment containing all the parameters associated with their value
		//in addition to all the current environment function definitions
		var execEv = ev.cloneFunDef
		(funDef.args zip args).foreach{ case (a,b) => execEv !+= a -> Some(b.valuation(ev)) }
		
		try {
			funDef.body.instructions.foreach(instr => instr.exec(execEv))
		} catch {
			case e : ReturnResultException => result = e.result
		}
		return result
	}
	
	def formatInstr(indent : String) : String = {
		var sb      = new StringBuilder
		var argsStr = {
			var i = 0
			args.foreach(arg => sb.append((if (i == 0) "" else ", ") + arg.formatExpr))
		}
		return "%s%s(%s)".format(indent,ident,args.addString(new StringBuilder,", "))
	}
	def formatExpr = formatInstr("")
}

class Return(expr : Expression) extends Instruction[Double] {
	def exec(ev: Environment)                = throw new ReturnResultException(expr.valuation(ev))
	def formatInstr(indent : String)         = "%sreturn %s;".format(indent,expr.formatExpr)
	def formatExpr                           = formatInstr("")
	private[instructions] def containsReturn = true
}

class MissingReturnException(ident : String) extends Exception("%s is missing a return instruction".format(ident))
class DuplicateParameterException extends Exception("All the parameters identifiers must be distinct")
private class ReturnResultException(val result : Double) extends Exception
