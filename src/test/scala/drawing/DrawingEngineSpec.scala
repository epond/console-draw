package drawing

import scalaz._
import org.specs2.mutable.Specification

class DrawingEngineSpec extends Specification {
  val blank3by2Canvas  = Canvas(
    """-----
      ||   |
      ||   |
      |-----""".stripMargin)

  val blank20by4Canvas = Canvas(
    """----------------------
      ||                    |
      ||                    |
      ||                    |
      ||                    |
      |----------------------""".stripMargin)

  "drawLayer" should {
    "Given an empty ColourLayer and empty Canvas then return the empty Canvas" in {
      val resultCanvas = DrawingEngine.drawLayer(emptyLayer, Canvas.empty)
      resultCanvas match {
        case \/-(canvas) => canvas must beEqualTo(Canvas.empty)
        case -\/(error)      => ko(error)
      }
    }

    "Given an empty ColourLayer and blank Canvas then return the blank Canvas" in {
      val resultCanvas = DrawingEngine.drawLayer(emptyLayer, blank20by4Canvas)
      resultCanvas match {
        case \/-(canvas) => canvas must beEqualTo(blank20by4Canvas)
        case -\/(error)  => ko(error)
      }
    }

    "Given a non-empty ColourLayer and blank Canvas then return the expected Canvas" in {
      val layer = (List(Coordinates(1, 1), Coordinates(2, 2)), 'x')
      val expectedCanvas = Canvas("""----------------------
                                    ||x                   |
                                    || x                  |
                                    ||                    |
                                    ||                    |
                                    |----------------------""".stripMargin)
      val resultCanvas = DrawingEngine.drawLayer(layer, blank20by4Canvas)
      resultCanvas match {
        case \/-(canvas) => canvas must beEqualTo(expectedCanvas)
        case -\/(error)  => ko(error)
      }
    }

    "Given a ColourLayer out of the Canvas bounds then return an error" in {
      DrawingEngine.drawLayer((List(Coordinates(4, 1)), 'x'), blank3by2Canvas) match {
        case \/-(canvas) => ko("Expected error was not returned")
        case -\/(error)  => error must beEqualTo("Out of bounds")
      }
      DrawingEngine.drawLayer((List(Coordinates(3,2), Coordinates(3, 3)), 'x'), blank3by2Canvas) match {
        case \/-(canvas) => ko("Expected error was not returned")
        case -\/(error)  => error must beEqualTo("Out of bounds")
      }
      DrawingEngine.drawLayer((List(Coordinates(3, 3)), 'x'), blank3by2Canvas) match {
        case \/-(canvas) => ko("Expected error was not returned")
        case -\/(error)  => error must beEqualTo("Out of bounds")
      }
    }
  }

  "applyCommand" should {
    "Given a NewCanvas command then return a blank canvas" in {
      val resultCanvas = DrawingEngine.applyCommand(NewCanvasCommand(20, 4), Canvas.empty)
      resultCanvas match {
        case \/-(canvas) => canvas must beEqualTo(blank20by4Canvas)
        case -\/(error)  => ko(error)
      }
    }

    "Given a DrawLine command on a blank canvas then the Canvas should contain the line" in {
      val expectedCanvas = Canvas("""----------------------
                                    ||                    |
                                    ||xxxxxx              |
                                    ||                    |
                                    ||                    |
                                    |----------------------""".stripMargin)
      val resultCanvas = DrawingEngine.applyCommand(DrawLineCommand(Coordinates(1,2), Coordinates(6,2)), blank20by4Canvas)
      resultCanvas match {
        case \/-(canvas) => canvas must beEqualTo(expectedCanvas)
        case -\/(error)  => ko(error)
      }
    }

    "Given a DrawLine command going left then the Canvas should contain the line" in {
      val expectedCanvas = Canvas("""----------------------
                                    ||                    |
                                    ||xxxxxx              |
                                    ||                    |
                                    ||                    |
                                    |----------------------""".stripMargin)
      val resultCanvas = DrawingEngine.applyCommand(DrawLineCommand(Coordinates(6,2), Coordinates(1,2)), blank20by4Canvas)
      resultCanvas match {
        case \/-(canvas) => canvas must beEqualTo(expectedCanvas)
        case -\/(error)  => ko(error)
      }
    }

    "Given a DrawLine command going up then the Canvas should contain the line" in {
      val expectedCanvas = Canvas("""----------------------
                                    || x                  |
                                    || x                  |
                                    ||                    |
                                    ||                    |
                                    |----------------------""".stripMargin)
      val resultCanvas = DrawingEngine.applyCommand(DrawLineCommand(Coordinates(2,2), Coordinates(2,1)), blank20by4Canvas)
      resultCanvas match {
        case \/-(canvas) => canvas must beEqualTo(expectedCanvas)
        case -\/(error)  => ko(error)
      }
    }

    "Given a DrawLine command then the Canvas should contain both old and new drawings" in {
      val initialCanvas =  Canvas("""----------------------
                                    ||                    |
                                    ||xxxxxx              |
                                    ||                    |
                                    ||                    |
                                    |----------------------""".stripMargin)
      val expectedCanvas = Canvas("""----------------------
                                    ||                    |
                                    ||xxxxxx              |
                                    ||     x              |
                                    ||     x              |
                                    |----------------------""".stripMargin)
      val resultCanvas = DrawingEngine.applyCommand(DrawLineCommand(Coordinates(6,3), Coordinates(6,4)), initialCanvas)
      resultCanvas match {
        case \/-(canvas) => canvas must beEqualTo(expectedCanvas)
        case -\/(error)  => ko(error)
      }
    }

    "Given a DrawLine command with an oblique line then an error should be returned" in {
      val resultCanvas = DrawingEngine.applyCommand(DrawLineCommand(Coordinates(1,2), Coordinates(3,4)), blank20by4Canvas)
      resultCanvas match {
        case \/-(canvas) => ko("Expected error was not returned")
        case -\/(error)  => error must beEqualTo("Only horizontal and vertical lines are supported")
      }
    }

    "Given a DrawRectangle command then the Canvas should contain the rectangle" in {
      val initialCanvas =  Canvas("""----------------------
                                    ||                    |
                                    ||xxxxxx              |
                                    ||     x              |
                                    ||     x              |
                                    |----------------------""".stripMargin)
      val expectedCanvas = Canvas("""----------------------
                                    ||               xxxxx|
                                    ||xxxxxx         x   x|
                                    ||     x         xxxxx|
                                    ||     x              |
                                    |----------------------""".stripMargin)
      val resultCanvas = DrawingEngine.applyCommand(DrawRectangleCommand(Coordinates(16,1), Coordinates(20,3)), initialCanvas)
      resultCanvas match {
        case \/-(canvas) => canvas must beEqualTo(expectedCanvas)
        case -\/(error)  => ko(error)
      }
    }

    "Given a BucketFill command in an open area then the Canvas should fill appropriately" in {
      val initialCanvas =  Canvas("""----------------------
                                    ||               xxxxx|
                                    ||xxxxxx         x   x|
                                    ||     x         xxxxx|
                                    ||     x              |
                                    |----------------------""".stripMargin)
      val expectedCanvas = Canvas("""----------------------
                                    ||oooooooooooooooxxxxx|
                                    ||xxxxxxooooooooox   x|
                                    ||     xoooooooooxxxxx|
                                    ||     xoooooooooooooo|
                                    |----------------------""".stripMargin)
      val resultCanvas = DrawingEngine.applyCommand(BucketFillCommand(Coordinates(10,3), 'o'), initialCanvas)
      resultCanvas match {
        case \/-(canvas) => canvas must beEqualTo(expectedCanvas)
        case -\/(error)      => ko(error)
      }
    }

    "Given a BucketFill command in a complicated open area then the Canvas should fill appropriately" in {
      val initialCanvas =  Canvas("""----------------------
                                    ||               xxxxx|
                                    ||xxxxxx         x   x|
                                    ||     x   xxxxx xxxxx|
                                    ||     x   x   x      |
                                    |----------------------""".stripMargin)
      val expectedCanvas = Canvas("""----------------------
                                    ||oooooooooooooooxxxxx|
                                    ||xxxxxxooooooooox   x|
                                    ||     xoooxxxxxoxxxxx|
                                    ||     xooox   xoooooo|
                                    |----------------------""".stripMargin)
      val resultCanvas = DrawingEngine.applyCommand(BucketFillCommand(Coordinates(10,2), 'o'), initialCanvas)
      resultCanvas match {
        case \/-(canvas) => canvas must beEqualTo(expectedCanvas)
        case -\/(error)      => ko(error)
      }
    }

    "Given a BucketFill command in a closed area then the Canvas should fill appropriately" in {
      val initialCanvas =  Canvas("""----------------------
                                    ||               xxxxx|
                                    ||xxxxxx         x   x|
                                    ||     x         xxxxx|
                                    ||     x              |
                                    |----------------------""".stripMargin)
      val expectedCanvas = Canvas("""----------------------
                                    ||               xxxxx|
                                    ||xxxxxx         xqqqx|
                                    ||     x         xxxxx|
                                    ||     x              |
                                    |----------------------""".stripMargin)
      val resultCanvas = DrawingEngine.applyCommand(BucketFillCommand(Coordinates(17,2), 'q'), initialCanvas)
      resultCanvas match {
        case \/-(canvas) => canvas must beEqualTo(expectedCanvas)
        case -\/(error)  => ko(error)
      }
    }

    "Given a BucketFill command on a non-empty origin position then do nothing" in {
      val initialCanvas =  Canvas("""----------------------
                                    ||               xxxxx|
                                    ||xxxxxx         x   x|
                                    ||     x         xxxxx|
                                    ||     x              |
                                    |----------------------""".stripMargin)
      val resultCanvas = DrawingEngine.applyCommand(BucketFillCommand(Coordinates(4,2), '0'), initialCanvas)
      resultCanvas match {
        case \/-(canvas) => canvas must beEqualTo(initialCanvas)
        case -\/(error)  => ko(error)
      }
    }

    "Given a Clear command with a non-empty Canvas then return a blank Canvas of the same dimensions" in {
      val initialCanvas =  Canvas("""----------------------
                                    ||               xxxxx|
                                    ||xxxxxx         x   x|
                                    ||     x         xxxxx|
                                    ||     x              |
                                    |----------------------""".stripMargin)
      val resultCanvas = DrawingEngine.applyCommand(ClearCommand, initialCanvas)
      resultCanvas match {
        case \/-(canvas) => canvas must beEqualTo(blank20by4Canvas)
        case -\/(error)  => ko(error)
      }
    }

  }
}
