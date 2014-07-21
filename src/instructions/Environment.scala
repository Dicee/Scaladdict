package instructions

class Environment extends Cloneable {
	private var env : Map[String,Option[Double]] = Map()
	private var fun : Map[String,Function]       = Map()
	
	private def this(env : Map[String,Option[Double]]) = {
		this()
		this.env = env
	}
	
	override def clone                                      = new Environment(env ++ Map())
	override def toString                                   = env.toString
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
		case None => throw new UndefinedVariableException(key)
	}
}

object Environment {
	def apply(keyVals : (String,Double)*) : Environment = {
		var result = new Environment();
		keyVals.foreach(keyVal => result += (keyVal._1,Some(keyVal._2)))
		return result;
	}
}

class UndefinedVariableException(var varName : String) extends Exception("Identifier %s is not declared".format(varName))
class DuplicateDefinitionException(var typeName : String, var varName : String) extends Exception("Duplicate %s %s".format(typeName,varName)) 
class UninitializedVariableException(var varName : String) extends Exception("Variable %s was declared but not initialized".format(varName))