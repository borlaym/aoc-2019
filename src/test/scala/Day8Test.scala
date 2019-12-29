import org.scalatest.FunSuite

class Day8Test extends FunSuite {
	test("Day8.layers") {
		assert(Day8.layers(List(1,2,3,4,5,6,7,8,9,0,1,2), 3, 2) == List(List(1,2,3,4,5,6), List(7,8,9,0,1,2)))
	}
	test("Day8.applyLayers") {
		assert(Day8.applyLayers(List(0,2,2,2,1,1,2,2,2,2,1,2,0,0,0,0), 2, 2) == List(0,1,1,0))
	}
}