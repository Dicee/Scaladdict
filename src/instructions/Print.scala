package instructions

class Print(s : String) extends Instruction[Unit] {
	def exec(ev : Environment)  : (Unit,Environment) = {
		println(s)
		return ({},ev)
	}
	def format(indent : String) : String = "%sprintln(%s)".format(indent,s)
}