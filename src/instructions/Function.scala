package instructions

import expressions.Expression
import expressions.Variable
import predicates.Predicate
import expressions.Value

class FunctionDef(val ident : String, val body : Block, val returnInstr : Expression, val args : Array[String])
	extends AbstractInstruction[Unit] {
	
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
		return other.ident == ident && other.args.sameElements(args)
	}
}

//class FunctionCall( extends EvaluableInstruction {
//	def exec(ev : Environment) : Double = 0
//	def formatInstr(indent : String) = "%s%s %s".format(indent,ident,)
//}

abstract class Parameter(val name : String)
case class Expr(s : String, expr : Expression) extends Parameter(s)
case class Pred(s : String, pred : Predicate ) extends Parameter(s)

class Test(val s : String) {
	override def equals(a : Any) : Boolean = {
		if (!a.isInstanceOf[Test]) return false
		return a.asInstanceOf[Test].s.equals(s)
	}
	override def toString = s
}

object Test {
	def main(args : Array[String]) = {
		var map : Map[Test,String] = Map()
		map += new Test("coucou") -> "coucou1"
		map += new Test("coucou") -> "coucou2"
		
		println(Array("coucou","salut").sameElements(Array("coucou","salut")))
	}
}