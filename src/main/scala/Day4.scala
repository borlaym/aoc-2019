object Day4 {
	def containsDouble(code: Int): Boolean = {
		code % 10 == (code / 10) % 10 ||
		(code / 10) % 10 == (code / 100) % 10 ||
		(code / 100) % 10 == (code / 1000) % 10 ||
		(code / 1000) % 10 == (code / 10000) % 10 ||
		(code / 10000) % 10 == (code / 100000) % 10
	}
	def getValidCodes() = {
		val allCodes = for {
			a <- 1 to 9
			b <- 1 to 9 if b >= a
			c <- 1 to 9 if c >= b
			d <- 1 to 9 if d >= c
			e <- 1 to 9 if e >= d
			f <- 1 to 9 if f >= e
		} yield 100000 * a + 10000 * b + 1000 * c + 100 * d + 10 * e + f
		allCodes.filter(containsDouble)
	}

	def main(args: Array[String]): Unit = {
		val MIN = 172851
		val MAX = 675869
		println(getValidCodes().filter(n => n > MIN && n < MAX).length)
	}
}
