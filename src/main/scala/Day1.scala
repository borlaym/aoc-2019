import scala.io.Source

object Day1 {
	def main(args: Array[String]): Unit = {
		val sources = Source.fromFile("src/day1Source.txt").getLines.toList.map(_.toInt)
		val sum = sources.map(getFuelRequirement(_)).foldLeft(0)((a: Int, b: Int) => a + b)
		println(sum)
	}

	def getFuelRequirement(mass: Int): Int = {
		val required = mass / 3 - 2
		if (required < 0) {
			0
		} else {
			required + getFuelRequirement(required)
		}
	}
}
