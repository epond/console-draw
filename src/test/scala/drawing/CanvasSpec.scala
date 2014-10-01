package drawing

import org.specs2.mutable.Specification

class CanvasSpec extends Specification {
  "A Canvas" should {
    "parse a string" in {
      val expectedCanvas = """----------------------
                             ||                    |
                             ||xxxxxx              |
                             ||     x              |
                             ||     x              |
                             |----------------------""".stripMargin
      val parsedCanvas = Canvas(expectedCanvas)
      parsedCanvas.toString must beEqualTo(expectedCanvas)
    }
  }
}
