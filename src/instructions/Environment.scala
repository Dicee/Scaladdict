package instructions

import scala.collection.mutable.HashSet
import scala.util.control.Breaks._

class Environment(private var env : Map[String,Option[Double]] = Map(), private var functions : Map[String,HashSet[FunctionDef]] = Map()) extends Cloneable {
	//Variables
	private def this(env : Map[String,Option[Double]]) = {
		this()
		this.env = env
	}
	
	override def clone                                      = new Environment(env ++ Map(),functions ++ Map())
	override def toString                                   = env.toString
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
	def containsDef(elt : FunctionDef) = functions.get(elt.ident) match { case Some(x) => x.contains(elt) ; case None => false }
	def defFunction(elt : FunctionDef) = 
	  functions.get(elt.ident) match {
		  	case Some(x) => if (!x.add(elt)) throw new DuplicateDefinitionException("function",elt.ident)
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