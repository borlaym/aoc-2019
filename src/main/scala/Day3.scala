final case class Segment (
     startX: Int,
     startY: Int,
     endX: Int,
     endY: Int
) {
	lazy val vector: (Int, Int) = (endX - startX, endY - startY)
	lazy val startPoint = (startX, startY)
	lazy val endPoint = (endX, endY)
}

object Day3 {
	def crossProduct(vector1: (Int, Int), vector2: (Int, Int)): Int = {
		vector1._1 * vector2._2 - vector1._2 * vector2._1
	}
	def intersection(segment1: Segment, segment2: Segment): Option[(Int, Int)] = {
		// Using cross product, from https://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect
		val r = segment1.vector
		val s = segment2.vector
		val qMinusP = (segment2.startX - segment1.startX, segment2.startY - segment1.startY)
		val uNumerator = crossProduct(qMinusP, r)
		val tNumerator = crossProduct(qMinusP, s)
		val denominator = crossProduct(r, s)
		if (denominator == 0) {
			None
		} else {
			val t = tNumerator.toDouble / denominator.toDouble
			val u = uNumerator.toDouble / denominator.toDouble
			if (t <= 1 && t >= 0 && u <=1 && u >= 0) {
				Some(((segment1.startX + t * segment1.vector._1).toInt, (segment1.startY + t * segment1.vector._2 ).toInt))
			} else {
				None
			}
		}

	}

	/**
	  * Given a start point and a command like U10, return a line segment
	  */
	def getSegment(startPoint: (Int, Int), command: String): Segment = {
		val parsedCommand = raw"([UDLR])(\d+)".r
		command match {
			case parsedCommand("U", amount) => new Segment(startPoint._1, startPoint._2, startPoint._1, startPoint._2 + amount.toInt)
			case parsedCommand("D", amount) => new Segment(startPoint._1, startPoint._2, startPoint._1, startPoint._2 - amount.toInt)
			case parsedCommand("L", amount) => new Segment(startPoint._1, startPoint._2, startPoint._1 - amount.toInt, startPoint._2)
			case parsedCommand("R", amount) => new Segment(startPoint._1, startPoint._2, startPoint._1 + amount.toInt, startPoint._2)
		}
	}

	def main(args: Array[String]): Unit = {

	}
}
