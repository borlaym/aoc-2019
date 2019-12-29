import scala.io.Source

object Day9 {
	def main(args: Array[String]): Unit = {
		val program = Source.fromFile("src/day9Source.txt").getLines().toList.map(_.split(",")).flatten.map(BigInt(_))
		val results = IntCode2.execute(program, List(BigInt(2)))
		println(results)
	}
}
