import org.scalatest.FunSuite

class Day6Test extends FunSuite {
	val orbits = Map(
		"B" -> "COM",
		"C" -> "B",
		"D" -> "C",
		"E" -> "D",
		"F" -> "E",
		"G" -> "B",
		"H" -> "G",
		"I" -> "D",
		"J" -> "E",
		"K" -> "J",
		"L" -> "K"
	)
	test("Day6.singleOrbit") {
		assert(Day6.singleOrbit(orbits, "D") == 3)
		assert(Day6.singleOrbit(orbits, "L") == 7)
		assert(Day6.calculateOrbits(orbits) == 42)
	}
}