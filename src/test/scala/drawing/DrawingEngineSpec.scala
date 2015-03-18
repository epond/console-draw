package drawing

import scalaz._
import Scalaz._
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
      DrawingEngine.drawLayer(emptyLayer, Canvas.empty) must beEqualTo(\/-(Canvas.empty))
    }

    "Given an empty ColourLayer and blank Canvas then return the blank Canvas" in {
      DrawingEngine.drawLayer(emptyLayer, blank20by4Canvas) must beEqualTo(\/-(blank20by4Canvas))
    }

    "Given a non-empty ColourLayer and blank Canvas then return the expected Canvas" in {
      val layer = (List(Coordinates(1, 1), Coordinates(2, 2)), 'x')
      val expectedCanvas = Canvas("""----------------------
                                    ||x                   |
                                    || x                  |
                                    ||                    |
                                    ||                    |
                                    |----------------------""".stripMargin)

      DrawingEngine.drawLayer(layer, blank20by4Canvas) must beEqualTo(\/-(expectedCanvas))
    }

    "Given a ColourLayer out of the Canvas bounds then return an error" in {
      DrawingEngine.drawLayer((List(Coordinates(4, 1)), 'x'), blank3by2Canvas) must beEqualTo(-\/("Out of bounds"))
      DrawingEngine.drawLayer((List(Coordinates(3,2), Coordinates(3, 3)), 'x'), blank3by2Canvas) must beEqualTo(-\/("Out of bounds"))
      DrawingEngine.drawLayer((List(Coordinates(3, 3)), 'x'), blank3by2Canvas) must beEqualTo(-\/("Out of bounds"))
    }
  }

  "applyCommand" should {
    "Given a NewCanvas command then return a blank canvas" in {
      DrawingEngine.applyCommand(NewCanvasCommand(20, 4))(Canvas.empty) must beEqualTo(\/-(blank20by4Canvas))
    }

    "Given a DrawLine command on a blank canvas then the Canvas should contain the line" in {
      val expectedCanvas = Canvas("""----------------------
                                    ||                    |
                                    ||xxxxxx              |
                                    ||                    |
                                    ||                    |
                                    |----------------------""".stripMargin)
      val command = DrawLineCommand(Coordinates(1, 2), Coordinates(6, 2))

      DrawingEngine.applyCommand(command)(blank20by4Canvas) must beEqualTo(\/-(expectedCanvas))
    }

    "Given a DrawLine command going left then the Canvas should contain the line" in {
      val expectedCanvas = Canvas("""----------------------
                                    ||                    |
                                    ||xxxxxx              |
                                    ||                    |
                                    ||                    |
                                    |----------------------""".stripMargin)
      val command = DrawLineCommand(Coordinates(6,2), Coordinates(1,2))

      DrawingEngine.applyCommand(command)(blank20by4Canvas) must beEqualTo(\/-(expectedCanvas))
    }

    "Given a DrawLine command going up then the Canvas should contain the line" in {
      val expectedCanvas = Canvas("""----------------------
                                    || x                  |
                                    || x                  |
                                    ||                    |
                                    ||                    |
                                    |----------------------""".stripMargin)
      val command = DrawLineCommand(Coordinates(2, 2), Coordinates(2, 1))

      DrawingEngine.applyCommand(command)(blank20by4Canvas) must beEqualTo(\/-(expectedCanvas))
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
      val command = DrawLineCommand(Coordinates(6, 3), Coordinates(6, 4))

      DrawingEngine.applyCommand(command)(initialCanvas) must beEqualTo(\/-(expectedCanvas))
    }

    "Given a DrawLine command with an oblique line then an error should be returned" in {
      val command = DrawLineCommand(Coordinates(1, 2), Coordinates(3, 4))

      DrawingEngine.applyCommand(command)(blank20by4Canvas) must beEqualTo(-\/("Only horizontal and vertical lines are supported"))
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
      val command = DrawRectangleCommand(Coordinates(16, 1), Coordinates(20, 3))

      DrawingEngine.applyCommand(command)(initialCanvas) must beEqualTo(\/-(expectedCanvas))
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
      val command = BucketFillCommand(Coordinates(10, 3), 'o')

      DrawingEngine.applyCommand(command)(initialCanvas) must beEqualTo(\/-(expectedCanvas))
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
      val command = BucketFillCommand(Coordinates(10, 2), 'o')

      DrawingEngine.applyCommand(command)(initialCanvas) must beEqualTo(\/-(expectedCanvas))
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
      val command = BucketFillCommand(Coordinates(17, 2), 'q')

      DrawingEngine.applyCommand(command)(initialCanvas) must beEqualTo(\/-(expectedCanvas))
    }

    "Given a BucketFill command on a non-empty origin position then do nothing" in {
      val initialCanvas =  Canvas("""----------------------
                                    ||               xxxxx|
                                    ||xxxxxx         x   x|
                                    ||     x         xxxxx|
                                    ||     x              |
                                    |----------------------""".stripMargin)
      val command = BucketFillCommand(Coordinates(4, 2), '0')

      DrawingEngine.applyCommand(command)(initialCanvas) must beEqualTo(\/-(initialCanvas))
    }

    "Given a BucketFill command on a very large canvas a stack overflow is not encountered" in {
      val canvasEither =
        Canvas.empty.right flatMap
          DrawingEngine.applyCommand(NewCanvasCommand(500, 500))_ flatMap
            DrawingEngine.applyCommand(BucketFillCommand(Coordinates(1, 1), 'o'))_

      canvasEither.isRight must beTrue
    }

    "Given a Clear command with a non-empty Canvas then return a blank Canvas of the same dimensions" in {
      val initialCanvas =  Canvas("""----------------------
                                    ||               xxxxx|
                                    ||xxxxxx         x   x|
                                    ||     x         xxxxx|
                                    ||     x              |
                                    |----------------------""".stripMargin)

      DrawingEngine.applyCommand(ClearCommand)(initialCanvas) must beEqualTo(\/-(blank20by4Canvas))
    }

  }
}
