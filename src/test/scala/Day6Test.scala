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
		"L" -> "K",
		"YOU" -> "K",
		"SAN" -> "I"
	)
	test("Day6.singleOrbit") {
		assert(Day6.singleOrbit(orbits, "D") == 3)
		assert(Day6.singleOrbit(orbits, "L") == 7)
		assert(Day6.calculateOrbits(orbits) == 54)
	}
	test("Day6.buildChain") {
		assert(Day6.buildChain(orbits, "YOU", List()) == List("COM", "B", "C", "D", "E", "J", "K"))
	}
	test("Day6.sharedSlice") {
		assert(Day6.sharedSlice(List("A", "B", "D"), List("A", "B", "C"), List()) == List("A", "B"))
	}
	test("Day6.distance") {
		val santa = Day6.buildChain(orbits, "SAN", List())
		val you = Day6.buildChain(orbits, "YOU", List())
		assert(Day6.distance(santa, you) == 4)
	}
}