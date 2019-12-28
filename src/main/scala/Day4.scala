object Day4 {
	def splitCode(code: Int): List[List[Char]] = {
		code.toString.toList.foldLeft(List[List[Char]]())((acc, char) => {
			acc match {
				case Nil => List(List(char))
				case head :: tail => if (head.head == char) (char :: head) :: tail else List(char) :: (head :: tail)
			}
		})
	}
	def containsDouble(code: Int): Boolean = {
		splitCode(code).exists(_.length == 2)
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
