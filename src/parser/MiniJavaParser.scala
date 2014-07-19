package parser

import scala.io.Source
import scala.util.parsing.combinator.JavaTokenParsers
import expressions.Expression
import expressions.Value
import instructions.Assignment
import instructions.Declaration
import instructions.Instruction
import predicates.Equal
import instructions.Environment
import expressions.Variable
import predicates.Predicate
import predicates.True
import predicates.False
import expressions.Ternary
import instructions.If
import instructions.Block

class ExpressionParser extends JavaTokenParsers {
	//Expressions
	type E = Expression

	def allExpr : Parser[E]    = tern | expr
	def expr    : Parser[E]    = term ~ rep(plus | minus)             ^^ { case a   ~ b   => (a /: b)((acc,f) => f(acc)) } 
	def term    : Parser[E]    = factor ~ rep(times | divide)         ^^ { case a   ~ b   => (a /: b)((acc,f) => f(acc)) }
	def plus    : Parser[E=>E] = "+" ~ term                           ^^ { case "+" ~ b   => _ + b                       }
	def minus   : Parser[E=>E] = "-" ~ term                           ^^ { case "-" ~ b   => _ - b                       }
	def times   : Parser[E=>E] = "*" ~ factor                         ^^ {case "*"  ~ b   => _ * b                       }
	def divide  : Parser[E=>E] = "/" ~ factor                         ^^ { case "/" ~ b   => _ / b                       } 
	def fpn     : Parser[E]    = floatingPointNumber                  ^^ { case x         => new Value(x.toDouble)       }
	def tern    : Parser[E]    = (pred <~ "?") ~ (expr <~ ":") ~ expr ^^ { case a ~ b ~ c => new Ternary(a,b,c)          } 
	def factor  : Parser[E]    = (
	      fpn 
	    | (ident ^^ { case x => new Variable(x) })
	    | "(" ~> tern <~ ")"
	    | "(" ~> expr <~ ")"  
	)

	//Predicates
	type P = Predicate
	
	//Double operators
	def le : Parser[P] = (expr <~ "<=") ~ expr ^^ { case e0 ~ e1 => e0 <= e1 }
	def lt : Parser[P] = (expr <~ "<" ) ~ expr ^^ { case e0 ~ e1 => e0 <  e1 }
	def ge : Parser[P] = (expr <~ ">=") ~ expr ^^ { case e0 ~ e1 => e0 >= e1 }
	def gt : Parser[P] = (expr <~ ">" ) ~ expr ^^ { case e0 ~ e1 => e0 >  e1 }
	def eq : Parser[P] = (expr <~ "==") ~ expr ^^ { case e0 ~ e1 => e0 == e1 }
	
	//Boolean operators
	def pred       : Parser[P]    = boolTerm ~ rep(or)    ^^ { case a    ~ b => (a /: b)((acc,f) => f(acc))  }
	def boolTerm   : Parser[P]    = boolFactor ~ rep(and) ^^ { case a    ~ b => (a /: b)((acc,f) => f(acc))  }
	def and        : Parser[P=>P] = "&&" ~ boolFactor     ^^ { case "&&" ~ b => _ && b                       }
	def or         : Parser[P=>P] = "||" ~ boolTerm       ^^ { case "||" ~ b => _ || b                       }
	def bool       : Parser[P]    = ("true" | "false")    ^^ { case "true"   => True ; case "false" => False }
	def boolFactor : Parser[P]    =  bool | "(" ~> pred <~ ")" | le | lt | ge | gt | eq | expr
}

class MiniJavaParser extends ExpressionParser {
	 private val fpt = floatingPointNumber
	 
	 def instr : Parser[Instruction[Any]] = ifStatement | block//assignment | declaration //| ifStatement | block
	 
	 def declaration = ("var" ~> ident ~ (("=" ~> allExpr)?) <~ ";") ^^ { 
		 	case x ~ Some(v) => new Declaration(x,Some(v)) 
		 	case x ~ None    => new Declaration(x,None)
	 	}
	 
	 
	 def assignment = (ident <~ "=") ~ allExpr <~ ";" ^^ { case x ~ y => new Assignment(x,y) }
	 def ifStatement = ("if" ~ "(" ~> pred <~ ")") ~ block ^^ { case a ~ b => new If(a -> b) }
	 
	 def block : Parser[Block] = "{" ~> (instr*) <~ "}" ^^ { case l => new Block(l.toSeq : _*) }
}

object Main extends MiniJavaParser {
	def main(args : Array[String]) : Unit = {
		var ev = Environment("gamma" -> 5)
		val lines = Source.fromFile("test0.txt").mkString
//		println(lines)
//		println(parseAll(pred, "5 > 7 && true || false && true || (true && 9 <= 9)").get.test(ev))
//		println(parseAll(expr,lines).get)
//		println(parseAll(allExpr,"4*(5 > 7 ? 5 : 3) + 5 > 5 ? 0 : 1").get.valuation(ev))
		println(parseAll(block,lines).get)
	}
}