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
import scala.collection.mutable.MutableList
import instructions.Switch
import instructions.PrintExpr
import instructions.PrintString

class ExpressionParser extends JavaTokenParsers {
	//Expressions
	type E  = Expression
	def fpn = floatingPointNumber
	
	def allExpr   : Parser[E]    = tern | expr
	def expr      : Parser[E]    = term ~ rep(plus | minus)             ^^ { case a   ~ b   => (a /: b)((acc,f) => f(acc)) } 
	def term      : Parser[E]    = factor ~ rep(times | divide)         ^^ { case a   ~ b   => (a /: b)((acc,f) => f(acc)) }
	def plus      : Parser[E=>E] = "+" ~ term                           ^^ { case "+" ~ b   => _ + b                       }
	def minus     : Parser[E=>E] = "-" ~ term                           ^^ { case "-" ~ b   => _ - b                       }
	def times     : Parser[E=>E] = "*" ~ factor                         ^^ { case "*"  ~ b  => _ * b                       }
	def divide    : Parser[E=>E] = "/" ~ factor                         ^^ { case "/" ~ b   => _ / b                       } 
	def variable  : Parser[E]    = ident                                ^^ { case x         => new Variable(x)             }
	def float     : Parser[E]    = fpn                                  ^^ { case x         => new Value(x.toDouble)       }
	def tern      : Parser[E]    = (pred <~ "?") ~ (expr <~ ":") ~ expr ^^ { case a ~ b ~ c => new Ternary(a,b,c)          } 
	def opposite  : Parser[E]    = "-" ~> opposable                     ^^ { case a         => a.opposite                  }
	def opposable : Parser[E]    = float | variable | ("(" ~> (expr | tern | term) <~ ")") 
	def factor    : Parser[E]    = float | variable | ("(" ~> (tern | expr) <~ ")") | opposite

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
}

class MiniJavaParser extends ExpressionParser {
	 private val fpt = floatingPointNumber
	 type I = Instruction[Any]
	 type B = Block
	 type D = Double
	 
	 def instr : Parser[I] = assignment | declaration | ifInstr | block
	 def block : Parser[B] = "{" ~> (instr*) <~ "}" ^^ { case l => new Block(l.toSeq : _*) }
	 
	 //Affectations
	 def declaration : Parser[I] = ("var" ~> ident ~ (("=" ~> allExpr)?) <~ ";") ^^ { 
		 	case x ~ Some(v) => new Declaration(x,Some(v)) 
		 	case x ~ None    => new Declaration(x,None)
	 	}
	 def assignment    : Parser[I]     = (ident <~ "=") ~ allExpr <~ ";" ^^ { case x ~ y => new Assignment(x,y) }

	 //If, else if, else
	 def ifInstr       : Parser[I]     = ifStatement ^^ { case a ~ b ~ c => new If(c,(List(a) ++ b).toSeq : _*) }
	 def elsif         : Parser[(P,B)] = "else " ~> test
	 def elseStatement : Parser[B]     = "else" ~> block
	 def ifStatement                   = test ~ rep(elsif) ~ (elseStatement?)
	 def test          : Parser[(P,B)] = (("if" ~ "(") ~> pred <~ ")") ~ block ^^ { case a ~ b => a -> b }
	 
	 //Switch 
	 def switch        : Parser[I]     = switchStruct                          ^^ { case a ~ b ~ c => new Switch(a,c,b.toSeq : _*)          } 
	 def caseClause    : Parser[(D,B)] = ("case" ~> fpn <~ ":") ~ rep(instr)   ^^ { case a ~ b     => a.toDouble -> new Block(b.toSeq : _*) }
	 def defaultClause : Parser[B]     = ("default" ~ ":") ~> rep(instr)       ^^ { case a         => new Block(a.toSeq : _*)               }
	 def switchStruct                  = (("switch" ~ "(") ~> expr <~ (")" ~ "{")) ~ rep(caseClause) ~ (defaultClause?) <~ "}"
	 
	 //Print
	 def print         : Parser[I]     = ("println" ~ "(") ~> (expr | ("\"" ~> "(.)*".r <~ "\"")) <~ ")" ^^ { 
		 	case a : Expression => new PrintExpr(a)
		 	case a : Any        => println(a) ; new PrintString(a)
	 	}
}

object Main extends MiniJavaParser {
	def main(args : Array[String]) : Unit = {
		var ev = Environment("gamma" -> 5)
		val lines = Source.fromFile("test3.txt").mkString
//		println(lines)
//		println(parseAll(pred, "5 > 7 && true || false && true || (true && 9 <= 9)").get.test(ev))
//		println(parseAll(expr,lines).get)
//		println(parseAll(allExpr,"4*(5 > 7 ? 5 : 3) + 5 > 5 ? 0 : 1").get.valuation(ev))
		
//		println(parseAll(pred,"(!(5 > 7) && (5 > 7)) || !false").get.test(ev))
//		println(parseAll(allExpr,"- 5 *(5 + 6) + 55").get.valuation(ev))
		println("c".matches("(\\w | \\W)"))
		println(parseAll(print,lines).get)
		println(parseAll(testou,"coucou").get)
	}
	
	def testou = "(.)*".r
}