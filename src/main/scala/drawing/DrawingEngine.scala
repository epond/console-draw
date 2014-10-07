package drawing

import scalaz._
import Scalaz._

object DrawingEngine {

  def applyCommand(command: CanvasCommand, canvas: Canvas): (String \/ Canvas) = command match {

    case NewCanvasCommand(width, height) => {
      Canvas(width, height).right
    }

    case DrawLineCommand(startPos, endPos) => {
      drawLine(startPos, endPos, canvas)
    }

    case DrawRectangleCommand(ulCorner: Coordinates, lrCorner: Coordinates) => {
      for {
        canvas1 <- drawLine(Coordinates(ulCorner.column, ulCorner.row), Coordinates(lrCorner.column, ulCorner.row), canvas)
        canvas2 <- drawLine(Coordinates(lrCorner.column, ulCorner.row), Coordinates(lrCorner.column, lrCorner.row), canvas1)
        canvas3 <- drawLine(Coordinates(lrCorner.column, lrCorner.row), Coordinates(ulCorner.column, lrCorner.row), canvas2)
        canvas4 <- drawLine(Coordinates(ulCorner.column, lrCorner.row), Coordinates(ulCorner.column, ulCorner.row), canvas3)
      } yield canvas4
    }

    case BucketFillCommand(origin: Coordinates, colour: Char) => {
      if (isOutOfBounds(origin, canvas)) {
        -\/("Out of bounds")
      } else {
        drawLayer(BucketFill.buildLayer(origin, colour, canvas), canvas)
      }
    }

    case ClearCommand => \/-(Canvas(canvas.width, canvas.height))

  }

  def drawLayer(layer: ColourLayer, canvas: Canvas): (String \/ Canvas) = {
    val (points, colour) = layer

    if (points.exists(isOutOfBounds(_, canvas))) {
      -\/("Out of bounds")
    } else {
      points.foldRight(canvas){(point, canv) => Canvas(
        for {
          rowWithIndex <- canv.rows.zipWithIndex
        } yield {
          val (row, rowIndex) = rowWithIndex
          if (rowIndex == point.row) {
            row.updated(point.column, colour)
          } else {
            row
          }
        })
      }.right
    }
  }

  private def drawLine(startPos: Coordinates, endPos: Coordinates, canvas: Canvas): (String \/ Canvas) = {
    val difference = endPos - startPos

    if (difference.row != 0 && difference.column != 0) {
      -\/("Only horizontal and vertical lines are supported")
    } else {
      val linePoints = if (difference.column != 0) {
        val step = if (difference.column > 0) 1 else -1
        (0 to difference.column by step).map(x => Coordinates(startPos.column + x, startPos.row))
      } else {
        val step = if (difference.row > 0) 1 else -1
        (0 to difference.row by step).map(x => Coordinates(startPos.column, startPos.row + x))
      }

      val layer = (linePoints, lineColour)
      drawLayer(layer, canvas)
    }
  }
}
