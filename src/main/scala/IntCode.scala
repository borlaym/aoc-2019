
object IntCode {
	def execute(list: List[Int], pointer: Int, inputStack: List[Int], output: List[Int]): List[Int] = {
		val operationData = list(pointer)
		val operation = operationData % 100
		// 0 means position mode (read from address), 1 means immediate mode (read value)
		val parameter1Mode = (operationData / 100) % 10
		val parameter2Mode = (operationData / 1000) % 10
		val parameter3Mode = (operationData / 10000) % 10
		operation match {
			// Addition
			case 1 => {
				val arg1 = list(pointer + 1)
				val arg2 = list(pointer + 2)
				val arg1Value = if (parameter1Mode == 0) list(arg1) else arg1
				val arg2Value = if (parameter2Mode == 0) list(arg2) else arg2
				val target = list(pointer + 3) // Target is never in immediate mode
				execute(list.updated(target, arg1Value + arg2Value), pointer + 4, inputStack, output)
			}
			// Multiplication
			case 2 => {
				val arg1 = list(pointer + 1)
				val arg2 = list(pointer + 2)
				val arg1Value = if (parameter1Mode == 0) list(arg1) else arg1
				val arg2Value = if (parameter2Mode == 0) list(arg2) else arg2
				val target = list(pointer + 3)
				execute(list.updated(target, arg1Value * arg2Value), pointer + 4, inputStack, output)
			}
			// Save Input
			case 3 => {
				val target = list(pointer + 1)
				execute(list.updated(target, inputStack.head), pointer + 2, inputStack.tail, output)
			}
			// Output
			case 4 => {
				val target = list(pointer + 1)
				execute(list, pointer + 2, inputStack, list(target) :: output)
			}
			// Jump-if-true
			case 5 => {
				val arg1 = list(pointer + 1)
				val arg2 = list(pointer + 2)
				val arg1Value = if (parameter1Mode == 0) list(arg1) else arg1
				val arg2Value = if (parameter2Mode == 0) list(arg2) else arg2
				if (arg1Value != 0) {
					execute(list, arg2Value, inputStack, output)
				} else {
					execute(list, pointer + 3, inputStack, output)
				}
			}
			// Jump-if-false
			case 6 => {
				val arg1 = list(pointer + 1)
				val arg2 = list(pointer + 2)
				val arg1Value = if (parameter1Mode == 0) list(arg1) else arg1
				val arg2Value = if (parameter2Mode == 0) list(arg2) else arg2
				if (arg1Value == 0) {
					execute(list, arg2Value, inputStack, output)
				} else {
					execute(list, pointer + 3, inputStack, output)
				}
			}
			// Less-than
			case 7 => {
				val arg1 = list(pointer + 1)
				val arg2 = list(pointer + 2)
				val arg1Value = if (parameter1Mode == 0) list(arg1) else arg1
				val arg2Value = if (parameter2Mode == 0) list(arg2) else arg2
				val target = list(pointer + 3) // Target is never in immediate mode
				val targetValue = if (arg1Value < arg2Value) 1 else 0
				execute(list.updated(target, targetValue), pointer + 4, inputStack, output)
			}
			// Equals
			case 8 => {
				val arg1 = list(pointer + 1)
				val arg2 = list(pointer + 2)
				val arg1Value = if (parameter1Mode == 0) list(arg1) else arg1
				val arg2Value = if (parameter2Mode == 0) list(arg2) else arg2
				val target = list(pointer + 3) // Target is never in immediate mode
				val targetValue = if (arg1Value == arg2Value) 1 else 0
				execute(list.updated(target, targetValue), pointer + 4, inputStack, output)
			}
			// Halt
			case 99 => output
			case _ => throw new Exception("Invalid input")
		}
	}
}
