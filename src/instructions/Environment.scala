package instructions

class Environment extends Cloneable {
	private var env : Map[String,Option[Double]] = Map()
	
	private def this(env : Map[String,Option[Double]]) = {
		this()
		this.env = env
	}
	
	override def clone                                      = new Environment(env ++ Map())
	def get(key : String)                                   = env.get(key)
	def contains(key : String)                              = env.contains(key)
	def iterator                                            = env.iterator
	override def toString                                   = env.toString
	def foreach(mapper : ((String,Option[Double])) => Unit) = env.foreach(mapper)
	def +=(kv : (String,Option[Double]))  : Environment     = { env += kv; return this; }
	def -=(key : String)          : Environment             = { env -= key; return this; }
	def !+=(kv : (String,Option[Double])) : Environment     = { 
		get(kv._1 ) match {
			case Some(value) => throw new DuplicateVariableException(kv._1)
			case None        => return this += kv
		}
	}
	def value(key : String) : Double = get(key) match {
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
class DuplicateVariableException(var varName : String) extends Exception("Duplicate variable %s".format(varName)) 
class UninitializedVariableException(var varName : String) extends Exception("Variable %s was declared but not initialized".format(varName))