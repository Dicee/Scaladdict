package interpreter

import scala.io.Source
import scala.util.parsing.combinator.JavaTokenParsers
import expressions._
import predicates._
import scala.collection.mutable.MutableList
import instructions._
import instructions.affectations._

class MiniJavaParser extends JavaTokenParsers {
	//Expressions
	type E  = Expression
	def fpn = floatingPointNumber
	
	///!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	///						      Don't forget to improve the inc/dec
	///!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	def allExpr      : Parser[E]    = inc | assignment | tern | expr 
	def expr         : Parser[E]    = term ~ rep(plus | minus)             ^^ { case a   ~ b   => (a /: b)((acc,f) => f(acc)) } 
	def term         : Parser[E]    = factor ~ rep(times | divide)         ^^ { case a   ~ b   => (a /: b)((acc,f) => f(acc)) }
	def plus         : Parser[E=>E] = "+" ~ term                           ^^ { case "+" ~ b   => _ + b                       }
	def minus        : Parser[E=>E] = "-" ~ term                           ^^ { case "-" ~ b   => _ - b                       }
	def times        : Parser[E=>E] = "*" ~ factor                         ^^ { case "*" ~ b   => _ * b                       }
	def divide       : Parser[E=>E] = "/" ~ factor                         ^^ { case "/" ~ b   => _ / b                       } 
	def variable     : Parser[E]    = ident                                ^^ { case x         => new Variable(x)             }
	def float        : Parser[E]    = fpn                                  ^^ { case x         => new Value(x.toDouble)       }
	def tern         : Parser[E]    = (pred <~ "?") ~ (expr <~ ":") ~ expr ^^ { case a ~ b ~ c => new Ternary(a,b,c)          } 
	def opposite     : Parser[E]    = "-" ~> unaryOperand                  ^^ { case a         => a.opposite                  }
	def intCast      : Parser[E]    = ("(" ~ "int" ~ ")") ~> unaryOperand  ^^ { case a         => new IntCast(a)              }
	def unaryOperand : Parser[E]    = float | variable | ("(" ~> (allExpr | term) <~ ")") 
	def factor       : Parser[E]    = float | funCall  | variable | opposite | intCast | ("(" ~> allExpr <~ ")") 

	//Predicates
	type P = Predicate
	
	//Double operators
	def le : Parser[P] = (expr <~ "<=") ~ expr ^^ { case e0 ~ e1 => e0 <= e1 }
	def lt : Parser[P] = (expr <~ "<" ) ~ expr ^^ { case e0 ~ e1 => e0 <  e1 }
	def ge : Parser[P] = (expr <~ ">=") ~ expr ^^ { case e0 ~ e1 => e0 >= e1 }
	def gt : Parser[P] = (expr <~ ">" ) ~ expr ^^ { case e0 ~ e1 => e0 >  e1 }
	def eq : Parser[P] = (expr <~ "==") ~ expr ^^ { case e0 ~ e1 => e0 == e1 }
	def ne : Parser[P] = (expr <~ "!=") ~ expr ^^ { case e0 ~ e1 => e0 != e1 }
	def op : Parser[P] =  le | lt | ge | gt | eq | ne | expr
	
	//Boolean operators
	def pred       : Parser[P]    = boolTerm ~ rep(or)    ^^ { case a    ~ b => (a /: b)((acc,f) => f(acc))  }
	def boolTerm   : Parser[P]    = boolFactor ~ rep(and) ^^ { case a    ~ b => (a /: b)((acc,f) => f(acc))  }
	def and        : Parser[P=>P] = "&&" ~ boolFactor     ^^ { case "&&" ~ b => _ && b                       }
	def or         : Parser[P=>P] = "||" ~ boolTerm       ^^ { case "||" ~ b => _ || b                       }
	def bool       : Parser[P]    = ("true" | "false")    ^^ { case "true"   => True ; case "false" => False }
	def neg        : Parser[P]    = "!" ~> negable        ^^ { case a => a.not }
	def boolFactor : Parser[P]    = bool | "(" ~> pred <~ ")" | op | neg
	def negable    : Parser[P]    = bool | "(" ~> (boolTerm | pred | op | allExpr) <~ ")"

	private val fpt = floatingPointNumber
	type I = Instruction[Any]
	type B = Block
	type D = Double
	 
	def allInstr = instr(block | returnBlock) | returnInstr
	def instr(block : Parser[B]) : Parser[I] = (
	      ((affectation | print | inc | funCall) <~ ";") 
		| ifInstr(block) 
		| block 
		| forInstr(block) 
		| switch(block) 
		| whileInstr(block)
	)
	def block : Parser[B] = "{" ~> (instr(block)*) <~ "}" ^^ { case l => new Block(l.toSeq : _*) }
	 
	//Affectations
	def declaration   : Parser[Declaration] = "var" ~> ident ~ (("=" ~> allExpr)?) ^^ { 
		 case x ~ Some(v) => new Declaration(x,Some(v)) 
		 case x ~ None    => new Declaration(x,None)
	 }
	def assignment                  = (ident ~ "(\\+|-|\\*|/|)=".r) ~ allExpr ^^ { 
		case a ~  "=" ~ b => new Assignment(a,b)
		case a ~ "+=" ~ b => new PlusAssignment(a,b) 
		case a ~ "-=" ~ b => new MinusAssignment(a,b) 
		case a ~ "*=" ~ b => new ProdAssignment(a,b) 
		case a ~ "/=" ~ b => new DivAssignment(a,b) 
	} 
	def affectation   : Parser[Affectation] = declaration | assignment
	
	//Increment, decrement
	def inc           : Parser[EvaluableInstruction] = rightInc | leftInc
	def rightInc      : Parser[EvaluableInstruction] = ident ~ "\\+\\+|--".r  ^^ { 
		case a ~ "++" => new Increment(a,true) 
		case a ~ "--" => new Decrement(a,true) 
	}
	def leftInc      : Parser[EvaluableInstruction] = "\\+\\+|--".r ~ ident  ^^ { 
		case "++" ~ a => new Increment(a,false) 
		case "--" ~ a => new Decrement(a,false) 
	}

	//If, else if, else
	def ifInstr(block : Parser[B])       = ifStatement(block) ^^ { case a ~ b ~ c => new If(c,(List(a) ++ b).toSeq : _*) }
	def elsif(block : Parser[B])         = "else " ~> test(block)
	def elseStatement(block : Parser[B]) = "else" ~> block
	def ifStatement(block : Parser[B])   = test(block) ~ rep(elsif(block)) ~ (elseStatement(block)?)
	def test(block : Parser[B])          = (("if" ~ "(") ~> pred <~ ")") ~ block ^^ { case a ~ b => a -> b }
	 
	//Switch 
	def switch(block : Parser[B])        = switchStruct(block) ^^ { case a ~ b ~ c => new Switch(a,c,b.toSeq : _*) } 
	def caseClause(block : Parser[B])    = ("case" ~> fpn <~ ":") ~ rep(instr(block)) ^^ { 
		case a ~ b     => a.toDouble -> new Block(b.toSeq : _*) 
	}
	def defaultClause(block : Parser[B]) = ("default" ~ ":") ~> rep(instr(block)) ^^ { case a => new Block(a.toSeq : _*) }
	def switchStruct(block : Parser[B])  =
		(("switch" ~ "(") ~> allExpr <~ (")" ~ "{")) ~ rep(caseClause(block)) ~ (defaultClause(block)?) <~ "}"
	 
	//Print
	def print                           = ("println" ~ "(") ~> (allExpr | "\"(.)*\"".r) <~ ")" ^^ { 
		 case a : Expression => new PrintExpr(a)
		 case a : String     => new PrintString(a.substring(1,a.length() - 1))
	 }
	 
	//For
	def forInstr(block : Parser[B])     = forStruct(block) ^^ { case a ~ b ~ c ~ d => {
		 	var result = new For(b,d.instructions.toSeq : _*);
		 	a match { case Some(init ~ last) => result.addInitClause  (init.:+(last).toSeq : _*); case None => }
		 	c match { case Some(init ~ last) => result.addUpdateClause(init.:+(last).toSeq : _*); case None => }
		 	result
	 	}
	}
	def initClause                      = (rep(affectation <~ ",") ~ affectation)?
	def update                          = assignment | inc
	def updateClause                    = (rep(update <~ ",") ~ update)?
	def forStruct(block : Parser[B])    = ("for" ~ "(") ~> (initClause <~ ";") ~ ((pred?) <~ ";") ~ (updateClause <~ ")") ~ block
	 
	//While
	def whileInstr(block : Parser[B])   = (("while" ~ "(") ~> pred <~ ")") ~ block ^^ { 
		case a ~ b => new While(a,b.instructions .toSeq : _*)
	}
	
	//Functions
	def funCall = (ident <~ "(") ~ (paramsCall <~ ")") ^^ { 
	  	case a ~ b => b match {
	  		case Some(c ~ d) => new FunctionCall(a,(c.:+(d)).toArray)
	  		case None        => new FunctionCall(a)
		}
	} 
	def funDef = ("function" ~> ident <~ "(") ~ (paramsDef <~ ")") ~ funBody ^^ { 
	  	case a ~ b ~ c => b match {
	  		case Some(d ~ e) => new FunctionDef(a,c,(d.:+(e)).toArray)
	  		case None        => new FunctionDef(a,c,Array())
	  	}
	}
	def paramsCall              = (rep(allExpr <~ ",") ~ allExpr)?
	def paramsDef               = (rep(ident <~ ",") ~ ident)?
	//This parser doesn't forces the block to contain a return instruction, however when the FunctionDef constructor is
	//called, this property will be checked and funDef will fail if there was no return
	def returnBlock : Parser[B] = "{" ~> rep(rep(instr(returnBlock)) ~ returnInstr ~ rep(instr(returnBlock))) <~ "}" ^^ { 
	  	case a => new Block(a.flatten{ case x ~ y ~ z => (x :+ y) ++ z }.toSeq : _*)
	}
	def funBody                 = "{" ~> (allInstr*) <~ "}"	^^ { case l => new Block(l.toSeq : _*) }
	def returnInstr             = "return" ~> allExpr <~ ";"                          ^^ { case a => new Return(a)           }
	
	//Program
	def program = (rep(funDef) <~ ("void" ~ "main" ~ "(" ~ ")")) ~ block ^^ { case a ~ b => new Program(b,a.toSeq : _*) }
}

object Main extends MiniJavaParser {
	def main(args : Array[String]) : Unit = {
		var ev = Environment("x" -> 5)
//		val lines = Source.fromFile("test9.txt").mkString
//		var prog = parseAll(program,lines).get
//		println(prog)
//		prog.exec
		
		println(parseAll(allExpr,"mod(n,2)").get)
		
//		var test   = Array(block,ifInstr(block),switch(returnBlock),forInstr(block),whileInstr(block),assignment <~ ";",
//				allExpr,funDef,funDef,program)
//		var i      = -1
//		var errors = 0
//		var input  = ""
//		test.foreach(p => 
//			try {
//				i    += 1
//				input = Source.fromFile("test%d.txt".format(i)).mkString
//				parseAll(p,input).get
//			} catch {
//				case _ => println("Test n°%d failed on input : %s".format(i,input)) ; errors += 1
//			})
//		if (errors == 0) println("All tests ran successfully !") 
	}
}