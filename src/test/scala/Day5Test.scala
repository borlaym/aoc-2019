import org.scalatest.FunSuite

class Day5Test extends FunSuite {
	test("Day5.execute") {
		// Equals to 8, position mode
		assert(IntCode.execute(List(3,9,8,9,10,9,4,9,99,-1,8), 0, List(8), List()) === List(1))
		assert(IntCode.execute(List(3,9,8,9,10,9,4,9,99,-1,9), 0, List(8), List()) === List(0))
		// Less than 8, position mode
		assert(IntCode.execute(List(3,9,7,9,10,9,4,9,99,-1,8), 0, List(2), List()) === List(1))
		assert(IntCode.execute(List(3,9,7,9,10,9,4,9,99,-1,8), 0, List(9), List()) === List(0))
		// Equal to 8, immediate mode
		assert(IntCode.execute(List(3,3,1108,-1,8,3,4,3,99), 0, List(8), List()) === List(1))
		assert(IntCode.execute(List(3,3,1108,-1,8,3,4,3,99), 0, List(9), List()) === List(0))
		// Less than 8, immediate mode
		assert(IntCode.execute(List(3,3,1107,-1,8,3,4,3,99), 0, List(2), List()) === List(1))
		assert(IntCode.execute(List(3,3,1107,-1,8,3,4,3,99), 0, List(9), List()) === List(0))
		// Is 0 with jumps
		assert(IntCode.execute(List(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9), 0, List(0), List()) === List(0))
		assert(IntCode.execute(List(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9), 0, List(8), List()) === List(1))
		// Is 0 with jumps, immediate mode
		assert(IntCode.execute(List(3,3,1105,-1,9,1101,0,0,12,4,12,99,1), 0, List(0), List()) === List(0))
		assert(IntCode.execute(List(3,3,1105,-1,9,1101,0,0,12,4,12,99,1), 0, List(8), List()) === List(1))
	}
}