import org.scalatest.FunSuite

class Day3Test extends FunSuite {
	test("Day3.intersection") {
		assert(Day3.intersection(
			new Segment(3, 0, 6, 0),
			new Segment(5, -2, 5, 2)
		) === Some((5, 0)))
		assert(Day3.intersection(
			new Segment(3, 1, 6, 1),
			new Segment(5, 2, 5, -2)
		) === Some((5, 1)))
	}
	test("Day3.getSegment") {
		assert(Day3.getSegment((2, 2), "U10") == new Segment(2, 2, 2, 12))
		assert(Day3.getSegment((2, 2), "L10") == new Segment(2, 2, -8, 2))
	}
}