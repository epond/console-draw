package drawing

import scalaz._

case class Coordinates(column: Int, row: Int) {
  def -(that: Coordinates): Coordinates = Coordinates(column - that.column, row - that.row)
}

object DrawingEngine {
  val lineColour = 'x'
  val rectangleColour = 'x'

  def drawLine(startPos: Coordinates, endPos: Coordinates, canvas: Canvas): (String \/ Canvas) = {
    val difference = endPos - startPos

    // assuming only horizontal and vertical lines are being drawn
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

  def applyCommand(command: CanvasCommand, canvas: Canvas): (String \/ Canvas) = command match {

    case NewCanvas(width, height) => {
      val edgeRow = Vector.fill(width+2)('-')
      val innerRow = '|' +: Vector.fill(width)(' ') :+ '|'
      \/-(Canvas(edgeRow +: Vector.fill(height)(innerRow) :+ edgeRow))
    }

    case DrawLine(startPos, endPos) => {
      drawLine(startPos, endPos, canvas)
    }

    case DrawRectangle(ulCorner: Coordinates, lrCorner: Coordinates) => {
      for {
        canvas1 <- drawLine(Coordinates(ulCorner.column, ulCorner.row), Coordinates(lrCorner.column, ulCorner.row), canvas)
        canvas2 <- drawLine(Coordinates(lrCorner.column, ulCorner.row), Coordinates(lrCorner.column, lrCorner.row), canvas1)
        canvas3 <- drawLine(Coordinates(lrCorner.column, lrCorner.row), Coordinates(ulCorner.column, lrCorner.row), canvas2)
        canvas4 <- drawLine(Coordinates(ulCorner.column, lrCorner.row), Coordinates(ulCorner.column, ulCorner.row), canvas3)
      } yield canvas4
    }

    case BucketFill(origin: Coordinates, colour: Char) => ??? // TODO

  }

  def drawLayer(layer: ColourLayer, canvas: Canvas): (String \/ Canvas) = {
    val (points, colour) = layer

    val isOutOfBounds: Coordinates => Boolean = (point) => {
      (point.column < 1 || point.column > canvas.rows(0).size-2) ||
      (point.row < 1    || point.row    > canvas.rows.size-2)
    }

    if (points.exists(isOutOfBounds(_))) {
      -\/("Out of bounds")
    } else {
      val resultCanvas = points.foldRight(canvas){(point, canv) => Canvas(
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
      }
      \/-(resultCanvas)
    }
  }
}
