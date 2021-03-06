object Day2 {
	def defaultProgram = List(
		1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,1,5,19,23,2,9,23,27,1,6,27,31,1,31,9,35,2,35,10,39,1,5,39,43,2,43,9,47,1,5,47,51,1,51,5,55,1,55,9,59,2,59,13,63,1,63,9,67,1,9,67,71,2,71,10,75,1,75,6,79,2,10,79,83,1,5,83,87,2,87,10,91,1,91,5,95,1,6,95,99,2,99,13,103,1,103,6,107,1,107,5,111,2,6,111,115,1,115,13,119,1,119,2,123,1,5,123,0,99,2,0,14,0
	)
	def main(args: Array[String]): Unit = {
		for (
			noun <- 0 until 99;
			verb <- 0 until 99
		) {
			if (tryInput(noun, verb)) {
				println(noun, verb)
			}
		}
	}
	def tryInput(noun: Int, verb: Int): Boolean = {
		val program = defaultProgram.updated(1, noun).updated(2, verb)
		val results = execute(program, 0)
		results(0) == 19690720
	}
	def execute(list: List[Int], pointer: Int): List[Int] = {
		val operation = list(pointer)
		operation match {
			case 1 => {
				val arg1 = list(pointer + 1)
				val arg2 = list(pointer + 2)
				val target = list(pointer + 3)
				execute(list.updated(target, list(arg1) + list(arg2)), pointer + 4)
			}
			case 2 => {
				val arg1 = list(pointer + 1)
				val arg2 = list(pointer + 2)
				val target = list(pointer + 3)
				execute(list.updated(target, list(arg1) * list(arg2)), pointer + 4)
			}
			case 99 => list
			case _ => throw new Exception("Invalud input")
		}
	}
}
