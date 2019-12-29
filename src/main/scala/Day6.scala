import scala.io.Source

object Day6 {
	def singleOrbit(allOrbits: Map[String, String], name: String): Int = {
		allOrbits.get(name) match {
			case None => 0
			case Some(around) => 1 + singleOrbit(allOrbits, around)
		}
	}
	def calculateOrbits(orbits: Map[String, String]): Int = {
		orbits.foldLeft[Int](0)({
			case (acc: Int, (name: String, around: String)) => {
				acc + singleOrbit(orbits, name)
			}
		})
	}
	def main(args: Array[String]): Unit = {
		val orbitRegex = raw"(\w{3})\)(\w{3})".r
		val sources = Source.fromFile("src/day6Source.txt").getLines.toList.map {
			case orbitRegex(around, name) => name -> around
		}.toMap
		val orbits = calculateOrbits(sources)
		println(orbits)
	}
}