package expressions

import instructions._
import predicates._
import instructions.affectations.Declaration
import instructions.affectations.Assignment

object Test {
	def main(args : Array[String]) : Unit = {
//		var v1 : Expression = new Value(5);
//		var v2 : Expression = new Value(10);
//		var v3 : Expression = new Value(13);
//		println(String.format("%s %s %s",v1,v2,v3))
//		printf("%s = %f\n",expr,expr.valuation(ev))
		
		var expr : Expression = new Opposite(new Dot(new Value(5),new Minus(new Variable("non"),new Variable("coucou"))))
		
		var declareCoucou : Declaration = new Declaration("coucou")
		var declareNon : Declaration = new Declaration("non",new Value(10))
		var alloc : Assignment = new Assignment("coucou",new Value(5))
		var predicate : Predicate = (new GreaterThan(new Value(5),new Value(2)) && new LowerThan(new Value(3),new Value(2))).not.not
		var declareResult : Declaration = new Declaration("result",new Ternary(predicate,expr,expr.opposite))
		//println("%s %b".format(predicate,predicate.test(ev)))
		
		var ev : Environment = Environment("coucou" -> 17,"enfant" -> 15)
		
		var switch : Switch = new Switch(expr,new Block(alloc,alloc),(5d,new Block(new Declaration("yoo",new Value(17)))),
				(10d,new Block(NOP)))
		//println(switch)
		
		var block : Block = new Block(alloc,declareNon,declareResult)
		
		var prog : Program = new Program(new Block(declareCoucou,declareNon,alloc,declareResult,switch))
		
		
//		println(prog)
		prog.exec
//		println(block)
//		println(ev)
//		println(block.exec(ev)._2)
		
//		var ev1 = Environment()
//		var ev2 = ev1.clone
//		ev2 += "a" -> 5
//		println(ev1 + " " + ev2)
		
		var expr2 = new Minus(new Variable("coucou"),new Dot(new Plus(alloc,new Value(10)),new Variable("coucou")))
		
//		println(expr2.valuation(Environment("coucou" -> 15)) + " " + expr2)
	}
}