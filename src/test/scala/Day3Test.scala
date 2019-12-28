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

	test("Day3.getSegments") {
		assert(Day3.getSegments("U10,R4") == List(new Segment(0, 0, 0, 10), new Segment(0, 10, 4, 10)))
	}

	test("Day3.wireLength") {
		assert(Day3.wireLength(
			List(new Segment(0, 0, 10, 0), new Segment(10, 0, 10, 5))
		) === 15)
	}
	test("Day3.wireLengthUntil") {
		assert(Day3.wireLengthUntil(
			List(new Segment(0, 0, 10, 0), new Segment(10, 0, 10, 5), new Segment(10, 5, 10, 10)),
			new Segment(10, 0, 10, 5),
			(10, 3)
		) === 13)
	}
}