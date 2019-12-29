import org.scalatest.FunSuite

class Day8Test extends FunSuite {
	test("Day8.layers") {
		assert(Day8.layers(List(1,2,3,4,5,6,7,8,9,0,1,2), 3, 2) == List(List(1,2,3,4,5,6), List(7,8,9,0,1,2)))
	}
}