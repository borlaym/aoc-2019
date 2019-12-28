import org.scalatest.FunSuite

class Day5Test extends FunSuite {
	test("Day5.execute") {
		assert(Day5.execute(List(1002,4,3,4, 33), 0, List()) === List(1002,4,3,4, 99))
	}
}