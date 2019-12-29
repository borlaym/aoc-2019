import scala.annotation.tailrec

/**
  * Because of very fundamental changes, made this into a new class
  */
object IntCode2 {
	def execute(list: List[BigInt], inputStack: List[BigInt]): List[BigInt] = {
		@tailrec
		def runOperation(list: Map[Int, BigInt], pointer: Int, inputStack: List[BigInt], output: List[BigInt], relativeBase: Int): List[BigInt] = {
			def listValue(index: Int): BigInt = list.getOrElse(index, BigInt(0))
			val operationData = list(pointer)
			val operation = (operationData % 100).toInt
			// 0 means position mode (read from address), 1 means immediate mode (read value)
			val parameter1Mode = ((operationData / 100) % 10).toInt
			val parameter2Mode = ((operationData / 1000) % 10).toInt
			operation match {
				// Addition
				case 1 => {
					val arg1 = listValue(pointer + 1)
					val arg2 = listValue(pointer + 2)
					val arg1Value = parameter1Mode match {
						case 0 => listValue(arg1.toInt)
						case 1 => arg1
						case 2 => listValue(relativeBase + arg1.toInt)
					}
					val arg2Value = parameter2Mode match {
						case 0 => listValue(arg2.toInt)
						case 1 => arg2
						case 2 => listValue(relativeBase + arg2.toInt)
					}
					val target = listValue(pointer + 3).toInt // Target is never in immediate mode
					runOperation(list.updated(target, arg1Value + arg2Value), pointer + 4, inputStack, output, relativeBase)
				}
				// Multiplication
				case 2 => {
					val arg1 = listValue(pointer + 1)
					val arg2 = listValue(pointer + 2)
					val arg1Value = parameter1Mode match {
						case 0 => listValue(arg1.toInt)
						case 1 => arg1
						case 2 => listValue(relativeBase + arg1.toInt)
					}
					val arg2Value = parameter2Mode match {
						case 0 => listValue(arg2.toInt)
						case 1 => arg2
						case 2 => listValue(relativeBase + arg2.toInt)
					}
					val target = listValue(pointer + 3).toInt
					runOperation(list.updated(target, arg1Value * arg2Value), pointer + 4, inputStack, output, relativeBase)
				}
				// Save Input
				case 3 => {
					val target = listValue(pointer + 1).toInt
					runOperation(list.updated(target, inputStack.head), pointer + 2, inputStack.tail, output, relativeBase)
				}
				// Output
				case 4 => {
					val arg1 = listValue(pointer + 1)
					val arg1Value = parameter1Mode match {
						case 0 => listValue(arg1.toInt)
						case 1 => arg1
						case 2 => listValue(relativeBase + arg1.toInt)
					}
					runOperation(list, pointer + 2, inputStack, output :+ arg1Value, relativeBase)
				}
				// Jump-if-true
				case 5 => {
					val arg1 = listValue(pointer + 1)
					val arg2 = listValue(pointer + 2)
					val arg1Value = parameter1Mode match {
						case 0 => listValue(arg1.toInt)
						case 1 => arg1
						case 2 => listValue(relativeBase + arg1.toInt)
					}
					val arg2Value = parameter2Mode match {
						case 0 => listValue(arg2.toInt)
						case 1 => arg2
						case 2 => listValue(relativeBase + arg2.toInt)
					}
					if (arg1Value != 0) {
						runOperation(list, arg2Value.toInt, inputStack, output, relativeBase)
					} else {
						runOperation(list, pointer + 3, inputStack, output, relativeBase)
					}
				}
				// Jump-if-false
				case 6 => {
					val arg1 = listValue(pointer + 1)
					val arg2 = listValue(pointer + 2)
					val arg1Value = parameter1Mode match {
						case 0 => listValue(arg1.toInt)
						case 1 => arg1
						case 2 => listValue(relativeBase + arg1.toInt)
					}
					val arg2Value = parameter2Mode match {
						case 0 => listValue(arg2.toInt)
						case 1 => arg2
						case 2 => listValue(relativeBase + arg2.toInt)
					}
					if (arg1Value == 0) {
						runOperation(list, arg2Value.toInt, inputStack, output, relativeBase)
					} else {
						runOperation(list, pointer + 3, inputStack, output, relativeBase)
					}
				}
				// Less-than
				case 7 => {
					val arg1 = listValue(pointer + 1)
					val arg2 = listValue(pointer + 2)
					val arg1Value = parameter1Mode match {
						case 0 => listValue(arg1.toInt)
						case 1 => arg1
						case 2 => listValue(relativeBase + arg1.toInt)
					}
					val arg2Value = parameter2Mode match {
						case 0 => listValue(arg2.toInt)
						case 1 => arg2
						case 2 => listValue(relativeBase + arg2.toInt)
					}
					val target = listValue(pointer + 3).toInt // Target is never in immediate mode
					val targetValue = if (arg1Value < arg2Value) 1 else 0
					runOperation(list.updated(target, targetValue), pointer + 4, inputStack, output, relativeBase)
				}
				// Equals
				case 8 => {
					val arg1 = listValue(pointer + 1)
					val arg2 = listValue(pointer + 2)
					val arg1Value = parameter1Mode match {
						case 0 => listValue(arg1.toInt)
						case 1 => arg1
						case 2 => listValue(relativeBase + arg1.toInt)
					}
					val arg2Value = parameter2Mode match {
						case 0 => listValue(arg2.toInt)
						case 1 => arg2
						case 2 => listValue(relativeBase + arg2.toInt)
					}
					val target = listValue(pointer + 3).toInt // Target is never in immediate mode
					val targetValue = if (arg1Value == arg2Value) 1 else 0
					runOperation(list.updated(target, targetValue), pointer + 4, inputStack, output, relativeBase)
				}
				// Change relative base value
				case 9 => {
					val arg1 = listValue(pointer + 1)
					val arg1Value = parameter1Mode match {
						case 0 => listValue(arg1.toInt)
						case 1 => arg1
						case 2 => listValue(relativeBase + arg1.toInt)
					}
					runOperation(list, pointer + 2, inputStack, output, relativeBase + arg1Value.toInt)
				}
				// Halt
				case 99 => output
				case _ => throw new Exception("Invalid input")
			}
		}

		runOperation(list.zipWithIndex.map{ case (value, key) => (key, value)}.toMap, 0, inputStack, List(), 0)
	}

	def main(args: Array[String]): Unit = {
		execute(List(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99).map(BigInt(_)), List())
	}
}
