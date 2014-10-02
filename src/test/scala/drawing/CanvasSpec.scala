package drawing

import org.specs2.mutable.Specification

class CanvasSpec extends Specification {
  val exampleCanvas = """----------------------
                        ||                    |
                        ||xxxxxx              |
                        ||     x              |
                        ||     x              |
                        |----------------------""".stripMargin
  "A Canvas" should {
    "implement parsing and toString as the inverse of each other" in {
      val parsedCanvas = Canvas(exampleCanvas)
      parsedCanvas.toString must beEqualTo(exampleCanvas)
    }

    "implement height" in {
      Canvas(exampleCanvas).height must beEqualTo(4)
    }

    "implement width" in {
      Canvas(exampleCanvas).width must beEqualTo(20)
    }

    "when getNode is called on an empty space then return the empty character" in {
      Canvas(exampleCanvas).getNode(Coordinates(1, 3)) must beEqualTo(Some(emptyPosition))
    }

    "when getNode is called on a line then return the line character" in {
      Canvas(exampleCanvas).getNode(Coordinates(6, 3)) must beEqualTo(Some('x'))
    }

    "when getNode is called out of bounds then return None" in {
      Canvas(exampleCanvas).getNode(Coordinates(1, 5)) must beNone
    }
  }
}
