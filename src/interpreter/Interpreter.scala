package interpreter

import instructions.Environment
import instructions.Instruction
import scala.util.parsing.combinator.JavaTokenParsers

object Interpreter extends MiniJavaParser {
	var ev : Environment = new Environment
	var continue = true
	
	//Input parsers
	def command = exit | help | loadDef | loadProg | execInstr | evaluatePred | evaluateExpr
	
	def exit      = "exit".r ^^ { _ => continue = false ; "Bye bye" }
	def help      = "help".r ^^ { _ => printHelp }
	def loadDef   = ("load" ~ "def") ~> path 
	def loadProg  = ("load" ~ "prog") ~> path
	def path      = "(.)*\\.scaladdict".r
	def execInstr = instr(block) ^^ { case a => a.exec(ev) ; println(ev) }
	def evaluatePred = pred ^^ { case a => println("Boolean = " + a.test(ev)) }
	def evaluateExpr = allExpr ^^ { case a => println("Double = " + a.valuation(ev)) }
	
	//Output
	def printHelp = println("help")
	
	def main(args : Array[String]) = {
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
		}
//		println(parseAll(loadDef,"load def coucou.scaladdict").get)
	}
}