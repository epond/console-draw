package drawing

import org.specs2.mutable.Specification

class CanvasSpec extends Specification {
  val emptyCanvas = """----------------------
                      ||                    |
                      ||                    |
                      ||                    |
                      ||                    |
                      |----------------------""".stripMargin
  val canvasWithTwoLines = """----------------------
                             ||                    |
                             ||xxxxxx              |
                             ||     x              |
                             ||     x              |
                             |----------------------""".stripMargin
  "A Canvas" should {
    "be creatable from width and height values" in {
      Canvas(20, 4).toString must beEqualTo(emptyCanvas)
    }
    "implement parsing and toString as the inverse of each other" in {
      Canvas(canvasWithTwoLines).toString must beEqualTo(canvasWithTwoLines)
    }

    "implement height" in {
      Canvas(canvasWithTwoLines).height must beEqualTo(4)
    }

    "implement width" in {
      Canvas(canvasWithTwoLines).width must beEqualTo(20)
    }

    "when getNode is called on an empty space then return the empty character" in {
      Canvas(canvasWithTwoLines).getNode(Coordinates(1, 3)) must beEqualTo(Some(emptyPosition))
    }

    "when getNode is called on a line then return the line character" in {
      Canvas(canvasWithTwoLines).getNode(Coordinates(6, 3)) must beEqualTo(Some('x'))
    }

    "when getNode is called out of bounds then return None" in {
      Canvas(canvasWithTwoLines).getNode(Coordinates(1, 5)) must beNone
    }
  }
}
