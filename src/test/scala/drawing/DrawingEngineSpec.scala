package drawing

import scalaz._
import org.specs2.mutable.Specification

class DrawingEngineSpec extends Specification {
  val empty3by2Canvas  = Canvas(
    """-----
      ||   |
      ||   |
      |-----""".stripMargin)

  val empty20by4Canvas = Canvas(
    """----------------------
      ||                    |
      ||                    |
      ||                    |
      ||                    |
      |----------------------""".stripMargin)

  "drawLayer" should {
    "Given an empty ColourLayer and empty Canvas then return the original Canvas" in {
      val initialCanvas = Canvas.empty
      val resultCanvas = DrawingEngine.drawLayer(emptyLayer, initialCanvas)
      resultCanvas match {
        case \/-(canvas) => canvas must beEqualTo(initialCanvas)
        case -\/(_)      => ko("No canvas")
      }
    }

    "Given an empty ColourLayer and non-empty Canvas then return the original Canvas" in {
      val resultCanvas = DrawingEngine.drawLayer(emptyLayer, empty20by4Canvas)
      resultCanvas match {
        case \/-(canvas) => canvas must beEqualTo(empty20by4Canvas)
        case -\/(error)  => ko(error)
      }
    }

    "Given a non-empty ColourLayer and non-empty Canvas then return the expected Canvas" in {
      val layer = (List(Coordinates(1, 1), Coordinates(2, 2)), 'x')
      val expectedCanvas = Canvas("""----------------------
                                    ||x                   |
                                    || x                  |
                                    ||                    |
                                    ||                    |
                                    |----------------------""".stripMargin)
      val resultCanvas = DrawingEngine.drawLayer(layer, empty20by4Canvas)
      resultCanvas match {
        case \/-(canvas) => canvas must beEqualTo(expectedCanvas)
        case -\/(error)  => ko(error)
      }
    }

    "Given a ColourLayer out of the Canvas bounds then return an error" in {
      DrawingEngine.drawLayer((List(Coordinates(4, 1)), 'x'), empty3by2Canvas) match {
        case \/-(canvas) => ko("Expected error was not returned")
        case -\/(error)  => error must beEqualTo("Out of bounds")
      }
      DrawingEngine.drawLayer((List(Coordinates(3,2), Coordinates(3, 3)), 'x'), empty3by2Canvas) match {
        case \/-(canvas) => ko("Expected error was not returned")
        case -\/(error)  => error must beEqualTo("Out of bounds")
      }
      DrawingEngine.drawLayer((List(Coordinates(3, 3)), 'x'), empty3by2Canvas) match {
        case \/-(canvas) => ko("Expected error was not returned")
        case -\/(error)  => error must beEqualTo("Out of bounds")
      }
    }
  }

  "applyCommand" should {
    "Given a NewCanvas command then return an empty canvas" in {
      val resultCanvas = DrawingEngine.applyCommand(NewCanvasCommand(20, 4), Canvas.empty)
      resultCanvas match {
        case \/-(canvas) => canvas must beEqualTo(empty20by4Canvas)
        case -\/(error)  => ko(error)
      }
    }

    "Given a DrawLine command on an empty canvas then the Canvas should contain the line" in {
      val expectedCanvas = Canvas("""----------------------
                                    ||                    |
                                    ||xxxxxx              |
                                    ||                    |
                                    ||                    |
                                    |----------------------""".stripMargin)
      val resultCanvas = DrawingEngine.applyCommand(DrawLineCommand(Coordinates(1,2), Coordinates(6,2)), empty20by4Canvas)
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
      val resultCanvas = DrawingEngine.applyCommand(DrawLineCommand(Coordinates(6,2), Coordinates(1,2)), empty20by4Canvas)
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
      val resultCanvas = DrawingEngine.applyCommand(DrawLineCommand(Coordinates(2,2), Coordinates(2,1)), empty20by4Canvas)
      resultCanvas match {
        case \/-(canvas) => canvas must beEqualTo(expectedCanvas)
        case -\/(error)  => ko(error)
      }
    }

    "Given a DrawLine command on a non-empty canvas then the Canvas should be as expected" in {
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

    "Given a DrawRectangle command on a non-empty canvas then the Canvas should contain the rectangle" in {
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

    "Given a BucketFill command on an occupied position then do nothing" in {
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

  }
}
