import org.scalatest.FunSuite

class Day1Test extends FunSuite {
	test("Day1.getFuelRequirement") {
		assert(Day1.getFuelRequirement(12) === 2)
		assert(Day1.getFuelRequirement(1969) === 966)
		assert(Day1.getFuelRequirement(100756) === 50346)
	}
}