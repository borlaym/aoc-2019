import org.scalatest.FunSuite

class Day2Test extends FunSuite {
	test("Day2.execute") {
		assert(Day2.execute(List(1,0,0,0,99), 0) === List(2,0,0,0,99))
		assert(Day2.execute(List(2,3,0,3,99), 0) === List(2,3,0,6,99))
		assert(Day2.execute(List(2,4,4,5,99,0), 0) === List(2,4,4,5,99,9801))
		assert(Day2.execute(List(1,1,1,4,99,5,6,0,99), 0) === List(30,1,1,4,2,5,6,0,99))
	}
}