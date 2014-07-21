package parser

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
	///									Don't forget the inc/dec
	///!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	def allExpr   : Parser[E]    = assignment | tern | expr 
	def expr      : Parser[E]    = term ~ rep(plus | minus)             ^^ { case a   ~ b   => (a /: b)((acc,f) => f(acc)) } 
	def term      : Parser[E]    = factor ~ rep(times | divide)         ^^ { case a   ~ b   => (a /: b)((acc,f) => f(acc)) }
	def plus      : Parser[E=>E] = "+" ~ term                           ^^ { case "+" ~ b   => _ + b                       }
	def minus     : Parser[E=>E] = "-" ~ term                           ^^ { case "-" ~ b   => _ - b                       }
	def times     : Parser[E=>E] = "*" ~ factor                         ^^ { case "*" ~ b   => _ * b                       }
	def divide    : Parser[E=>E] = "/" ~ factor                         ^^ { case "/" ~ b   => _ / b                       } 
	def variable  : Parser[E]    = ident                                ^^ { case x         => new Variable(x)             }
	def float     : Parser[E]    = fpn                                  ^^ { case x         => new Value(x.toDouble)       }
	def tern      : Parser[E]    = (pred <~ "?") ~ (expr <~ ":") ~ expr ^^ { case a ~ b ~ c => new Ternary(a,b,c)          } 
	def opposite  : Parser[E]    = "-" ~> opposable                     ^^ { case a         => a.opposite                  }
	def opposable : Parser[E]    = float | variable | ("(" ~> (allExpr | term) <~ ")") 
	def factor    : Parser[E]    = float | variable | opposite | ("(" ~> allExpr <~ ")") 

	//Predicates
	type P = Predicate
	
	//Double operators
	def le : Parser[P] = (expr <~ "<=") ~ expr ^^ { case e0 ~ e1 => e0 <= e1 }
	def lt : Parser[P] = (expr <~ "<" ) ~ expr ^^ { case e0 ~ e1 => e0 <  e1 }
	def ge : Parser[P] = (expr <~ ">=") ~ expr ^^ { case e0 ~ e1 => e0 >= e1 }
	def gt : Parser[P] = (expr <~ ">" ) ~ expr ^^ { case e0 ~ e1 => e0 >  e1 }
	def eq : Parser[P] = (expr <~ "==") ~ expr ^^ { case e0 ~ e1 => e0 == e1 }
	def op : Parser[P] =  le | lt | ge | gt | eq | expr
	
	//Boolean operators
	def pred       : Parser[P]    = boolTerm ~ rep(or)    ^^ { case a    ~ b => (a /: b)((acc,f) => f(acc))  }
	def boolTerm   : Parser[P]    = boolFactor ~ rep(and) ^^ { case a    ~ b => (a /: b)((acc,f) => f(acc))  }
	def and        : Parser[P=>P] = "&&" ~ boolFactor     ^^ { case "&&" ~ b => _ && b                       }
	def or         : Parser[P=>P] = "||" ~ boolTerm       ^^ { case "||" ~ b => _ || b                       }
	def bool       : Parser[P]    = ("true" | "false")    ^^ { case "true"   => True ; case "false" => False }
	def neg        : Parser[P]    = "!" ~> negable        ^^ { case a => a.not }
	def boolFactor : Parser[P]    = bool | "(" ~> pred <~ ")" | op | neg
	def negable    : Parser[P]    = bool | "(" ~> (boolTerm | pred | op) <~ ")"

	private val fpt = floatingPointNumber
	type I = Instruction[Any]
	type B = Block
	type D = Double
	 
	def instr : Parser[I] = ((affectation | print | inc) <~ ";") | ifInstr | block | forInstr | switch
	def block : Parser[B] = "{" ~> (instr*) <~ "}" ^^ { case l => new Block(l.toSeq : _*) }
	 
	//Affectations
	def declaration   : Parser[Declaration] = "var" ~> ident ~ (("=" ~> allExpr)?) ^^ { 
		 case x ~ Some(v) => new Declaration(x,Some(v)) 
		 case x ~ None    => new Declaration(x,None)
	 }
	def assignment                  = (ident ~ "(\\+|-|\\*|/|)=".r) ~ allExpr ^^ { 
		case a ~  "=" ~ b => new Assignment(a,b);
		case a ~ "+=" ~ b => new PlusAssignment(a,b) 
		case a ~ "-=" ~ b => new MinusAssignment(a,b) 
		case a ~ "*=" ~ b => new ProdAssignment(a,b) 
		case a ~ "/=" ~ b => new DivAssignment(a,b) 
	}
	def affectation   : Parser[Affectation] = declaration | assignment
	
	//Increment, decrement
	def inc           : Parser[EvaluableInstruction] = rightInc | leftInc
	def rightInc      : Parser[EvaluableInstruction] = ident ~ "\\+\\+|--".r ^^ { 
		case a ~ "++" => new Increment(a,true) 
		case a ~ "--" => new Decrement(a,true) 
	}
	def leftInc      : Parser[EvaluableInstruction] = "\\+\\+|--".r ~ ident  ^^ { 
		case "++" ~ a => new Increment(a,false) 
		case "--" ~ a => new Decrement(a,false) 
	}

	//If, else if, else
	def ifInstr       : Parser[I]     = ifStatement                           ^^ { case a ~ b ~ c => new If(c,(List(a) ++ b).toSeq : _*) }
	def elsif         : Parser[(P,B)] = "else " ~> test
	def elseStatement : Parser[B]     = "else" ~> block
	def ifStatement                   = test ~ rep(elsif) ~ (elseStatement?)
	def test          : Parser[(P,B)] = (("if" ~ "(") ~> pred <~ ")") ~ block ^^ { case a ~ b => a -> b }
	 
	//Switch 
	def switch        : Parser[I]     = switchStruct                          ^^ { case a ~ b ~ c => new Switch(a,c,b.toSeq : _*)          } 
	def caseClause    : Parser[(D,B)] = ("case" ~> fpn <~ ":") ~ rep(instr)   ^^ { case a ~ b     => a.toDouble -> new Block(b.toSeq : _*) }
	def defaultClause : Parser[B]     = ("default" ~ ":") ~> rep(instr)       ^^ { case a         => new Block(a.toSeq : _*)               }
	def switchStruct                  = (("switch" ~ "(") ~> allExpr <~ (")" ~ "{")) ~ rep(caseClause) ~ (defaultClause?) <~ "}"
	 
	//Print
	def print         : Parser[I]     = ("println" ~ "(") ~> (allExpr | "\"(.)*\"".r) <~ ")" ^^ { 
		 case a : Expression => new PrintExpr(a)
		 case a : String     => new PrintString(a.substring(1,a.length() - 1))
	 }
	 
	//For
	def forInstr     : Parser[I]      = forStruct ^^ { case a ~ b ~ c ~ d => {
		 	var result = new For(b,d.instructions.toSeq : _*);
		 	a match { case Some(init ~ last) => result.addInitClause  (init.:+(last).toSeq : _*); case None => }
		 	c match { case Some(init ~ last) => result.addUpdateClause(init.:+(last).toSeq : _*); case None => }
		 	result
	 	}
	}
	def initClause                    = (rep(affectation <~ ",") ~ affectation)?
	def updateClause                  = (rep(assignment <~ ",") ~ assignment)?
	def forStruct                     = ("for" ~ "(") ~> (initClause <~ ";") ~ ((pred?) <~ ";") ~ (updateClause <~ ")") ~ block
	 
	//While
	def whileInstr   : Parser[I]      = (("while" ~ "(") ~> pred <~ ")") ~ block ^^ { case a ~ b => new While(a,b.instructions .toSeq : _*)}
}

object Main extends MiniJavaParser {
	def main(args : Array[String]) : Unit = {
		var ev = Environment("gamma" -> 5)
		val lines = Source.fromFile("test6.txt").mkString
//		println(lines)
//		println(parseAll(pred, "5 > 7 && true || false && true || (true && 9 <= 9)").get.test(ev))
//		println(parseAll(expr,lines).get)
//		println(parseAll(allExpr,"4*(5 > 7 ? 5 : 3) + 5 > 5 ? 0 : 1").get.valuation(ev))
		
//		println(parseAll(pred,"(!(5 > 7) && (5 > 7)) || !false").get.test(ev))
//		println(parseAll(allExpr,"- 5 *(5 + 6) + 55").get.valuation(ev))
//		println("c".matches("(\\w | \\W)"))
//		println(parseAll(print,lines).get)
//		println(parseAll(testou,"coucou").get)
//		println(parseAll(whileInstr,lines).get)
		println(parseAll(allExpr,lines).get)
	}
}