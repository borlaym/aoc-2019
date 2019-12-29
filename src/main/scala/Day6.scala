import scala.io.Source

object Day6 {
	def buildChain(allOrbits: Map[String, String], name: String, chain: List[String]): List[String] = {
		allOrbits.get(name) match {
			case None => chain
			case Some(around) => buildChain(allOrbits, around, around :: chain)
		}
	}
	def sharedSlice(list1: List[String], list2: List[String], shared: List[String]): List[String] = {
		list1 match {
			case head :: _ if list2.head == head => sharedSlice(list1.tail, list2.tail, shared :+ head)
			case _ => shared
		}
	}
	def distance(from: List[String], to: List[String]): Int = {
		val shared = sharedSlice(from, to, List())
		from.drop(shared.length).length + to.drop(shared.length).length
	}
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

		val santaChain = buildChain(sources, "SAN", List())
		val youChain = buildChain(sources, "YOU", List())
		println(distance(santaChain, youChain))
	}
}