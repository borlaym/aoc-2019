import scala.io.Source

object Day8 {
	def layers(imageData: List[Int], width: Int, height: Int): List[List[Int]] = {
		val layerLength = width * height
		imageData.grouped(layerLength).toList
	}

	def applyLayers(imageData: List[Int], width: Int, height: Int): List[Int] = {
		val allLayers = layers(imageData, width, height)
		allLayers.init.foldRight(allLayers.last)((layer, image) => {
			(image zip layer).map {
				case (below, 2) => below
				case (_, above) => above
			}
		})
	}

	def main(args: Array[String]): Unit = {
		val imageData = Source.fromFile("src/day8Source.txt").toList.map(_.toString).map(_.toInt)
		val layered = layers(imageData, 25, 6)
		val targetLayer = layered.minBy((layer: List[Int]) => layer.count(_ == 0))
		println(targetLayer.count(_ == 1) * targetLayer.count(_ == 2))

		applyLayers(imageData, 25, 6).grouped(25).foreach(println(_))
		// ZLJBF
	}
}
