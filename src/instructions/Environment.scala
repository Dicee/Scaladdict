package instructions

import scala.collection.mutable.HashSet
import scala.util.control.Breaks._
import expressions.Value

class Environment(private var env : Map[String,Option[Double]] = Map(), private var functions : Map[String,HashSet[FunctionDef]] = Map()) extends Cloneable {
	//Variables
	private def this(env : Map[String,Option[Double]]) = {
		this()
		this.env = env
	}
	
	override def clone                                      = new Environment(env ++ Map(),functions ++ Map())
	override def toString                                   = {
		"Environment = " + env.addString(new StringBuilder,"(",", ",")").toString
	}
	def cloneFunDef                                         = new Environment(Map(),functions ++ Map())
	def iterator                                            = env.iterator
	def foreach(mapper : ((String,Option[Double])) => Unit) = env.foreach(mapper)
	def contains(key : String)                              = env.contains(key)
	def +=(kv : (String,Option[Double]))     	            = { env += kv;                   this; }
	def +=(s  : String)                                     = { env += s     -> None;        this; }
	def -=(key : String)                                    = { env -= key;                  this; }
	def !+=(kv : (String,Option[Double])) = { 
		env.get(kv._1 ) match {
			case Some(value) => throw new DuplicateDefinitionException("variable",kv._1)
			case None        => this += kv
		}
	}
	def get(key : String) : Double = env.get(key) match {
		case Some(optionValue) => optionValue match {
			case Some(d) => d
			case None    => throw new UninitializedVariableException(key)
		}
		case None => throw new UndeclaredIdentifierException(key)
	}
	
	//Functions
	def containsDef(elt : FunctionDef)        = functions.get(elt.ident) match { case Some(x) => x.contains(elt) ; case None => false }
	def defFunction(elt : FunctionDef) : Unit = defFunction(elt,false)
	
	def defFunction(elt : FunctionDef, overriding : Boolean) : Unit =
	  functions.get(elt.ident) match {
		  	case Some(x) => 
		  	  if (!x.add(elt)) 
		  		  if (overriding) {
		  			  //It's a bit hacky but it works... The previous definition is removed because its signature
		  			  //is equal to elt's one, then elt is added, bringing a new body. We could have used the find
		  			  //method and then set a new body to the result (if any) but the code would be longer and I'm 
		  		      //not sure it would be as performant as this solution, considering that a HashSet implements
		  			  //remove and add in a constant time whereas a find call is more likely to have a O(n) complexity
		  			  x.remove(elt)
		  			  x.add(elt)
		  		  } else
		  		    throw new DuplicateDefinitionException("function",elt.ident)
		  	case None    => functions += elt.ident -> HashSet(elt)
		} 
	
	def getDef(key : String, nParams : Int) = 
		functions.get(key) match {
			case Some(x) => x.find(funDef => funDef.args.length == nParams) match {
			  	case Some(y) => y
			  	case None    => throw new ArityException(key,nParams)
			}
			case None    => throw new UndeclaredIdentifierException(key)
		}
}		
	  
object Environment {
	def apply(keyVals : (String,Double)*) : Environment = {
		var result = new Environment();
		keyVals.foreach(keyVal => result += (keyVal._1,Some(keyVal._2)))
		return result;
	}
}

class UndeclaredIdentifierException(ident : String) extends Exception("Identifier %s is not declared".format(ident))
class DuplicateDefinitionException(typeName : String, var varName : String) extends Exception("Duplicate %s %s".format(typeName,varName)) 
class UninitializedVariableException(varName : String) extends Exception("Variable %s was declared but not initialized".format(varName))
class ArityException(ident : String, arity : Int) extends Exception("%s(args... [%s]) was not found".format(ident,arity))