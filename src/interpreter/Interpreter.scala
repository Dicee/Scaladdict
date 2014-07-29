package interpreter

import scala.io.Source
import scala.util.parsing.combinator.JavaTokenParsers
import instructions.Environment
import instructions.FunctionDef
import instructions.Instruction
import instructions.Program
import instructions.FunctionCall
import expressions.Value

object Interpreter extends MiniJavaParser {
	var ev : Environment = new Environment
	var continue = true
	
	//Input parsers
	def command = exit | help | loadDef | loadProg | execInstr | evaluateExpr | evaluatePred  | defFunction
	
	def exit      = "exit".r ^^ { _ => continue = false ; "Bye bye" }
	def help      = "help".r ^^ { _ => printHelp }
	def loadDef   = ("load" ~ "def") ~> path ^^ { case a => loadFunDef(a) }
	def funDefList = (program | rep(funDef)) ^^ { case a : Program => a.functions ; case a : List[FunctionDef] => a }
	def loadProg  = ("exec" ~ "prog") ~> path ^^ { case a => parseAll(program,Source.fromFile(a).mkString).get.exec }
	def path      = "(.)*\\.scaladdict".r
	def execInstr = instr(block) ^^ { case a => a.exec(ev) ; println(ev) }
	def evaluatePred = pred ^^ { case a => println("Boolean = " + a.test(ev)) }
	def evaluateExpr = allExpr ^^ { case a => println("Double = " + a.valuation(ev)) }
	def defFunction = "def" ~> funDef
	
	//Processing based on the parsed values
	def printHelp = println("help")
	def loadFunDef(s : String) = parseAll(funDefList,Source.fromFile(s).mkString).get
		.foreach(f => { ev.defFunction(f,true) ; println("Defined %s(args... [%s])".format(f.ident,f.args.length)) })
	
	def main(args : Array[String]) = {
	  /*
		var instr = ""
		while (continue) {
			try {
				var line = Console.readLine
				if (line.endsWith("..")) 
					instr += line.substring(0,line.length - 2)
				else {
					instr += line
					parseAll(command,instr)
					instr = ""
				}
			} catch {
			  	case e : Throwable => println(e.getMessage)
			  	instr = ""
			}
		}*/
		println(parseAll(loadDef,"load def C:/Users/David/Desktop/a.scaladdict").get)
		println(ev.getDef("squaringExponentiation",2))
		println(ev.getDef("mod",2))
		var call = new FunctionCall("squaringExponentiation",Array(new Value(1.5),new Value(2)))
		println(call.exec(ev))
	}
}