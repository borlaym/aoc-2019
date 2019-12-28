import org.scalatest.FunSuite

class Day4Test extends FunSuite {
	test("Day4.containsDouble") {
		assert(Day4.containsDouble(123455) == true)
		assert(Day4.containsDouble(123445) == true)
		assert(Day4.containsDouble(123345) == true)
		assert(Day4.containsDouble(122345) == true)
		assert(Day4.containsDouble(112345) == true)
		assert(Day4.containsDouble(123456) == false)
	}

}