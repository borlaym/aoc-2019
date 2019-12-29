import scala.io.Source

object Day9 {
	def main(args: Array[String]): Unit = {
		val program = Source.fromFile("src/day9Source.txt").toString().split(",")
		println(program)
	}
}
