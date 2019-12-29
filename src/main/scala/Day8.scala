import scala.io.Source

object Day8 {
	def layers(imageData: List[Int], width: Int, height: Int): List[List[Int]] = {
		val layerLength = width * height
		imageData.grouped(layerLength).toList
	}

	def main(args: Array[String]): Unit = {
		val imageData = Source.fromFile("src/day8Source.txt").toList.map(_.toString).map(_.toInt)
		val layered = layers(imageData, 25, 6)
		val targetLayer = layered.minBy((layer: List[Int]) => layer.count(_ == 0))
		println(targetLayer.count(_ == 1) * targetLayer.count(_ == 2))
	}
}
