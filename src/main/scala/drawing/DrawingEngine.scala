package drawing

import bettereither._

object DrawingEngine {

  def applyCommand(command: CanvasCommand)(canvas: Canvas): (String \/ Canvas) = command match {

    case NewCanvasCommand(width, height) =>
      \/-(Canvas(width, height))

    case DrawLineCommand(startPos, endPos) =>
      drawLine(startPos, endPos)(canvas)

    case DrawRectangleCommand(ulCorner: Coordinates, lrCorner: Coordinates) =>
      \/-(canvas) flatMap
        drawLine(Coordinates(ulCorner.column, ulCorner.row), Coordinates(lrCorner.column, ulCorner.row)) flatMap
        drawLine(Coordinates(lrCorner.column, ulCorner.row), Coordinates(lrCorner.column, lrCorner.row)) flatMap
        drawLine(Coordinates(lrCorner.column, lrCorner.row), Coordinates(ulCorner.column, lrCorner.row)) flatMap
        drawLine(Coordinates(ulCorner.column, lrCorner.row), Coordinates(ulCorner.column, ulCorner.row))

    case BucketFillCommand(origin: Coordinates, colour: Char) =>
      if (isOutOfBounds(origin, canvas)) {
        -\/("Out of bounds")
      } else {
        drawLayer(BucketFill.buildLayer(origin, colour, canvas), canvas)
      }

    case ClearCommand =>
      \/-(Canvas(canvas.width, canvas.height))

  }

  def drawLayer(layer: ColourLayer, canvas: Canvas): (String \/ Canvas) = {
    if (layer.points.exists(isOutOfBounds(_, canvas))) {
      -\/("Out of bounds")
    } else {
      \/-(layer.points.foldLeft(canvas)(plot(layer.colour)))
    }
  }

  def plot(colour: Char)(canv: Canvas, point: Coordinates): Canvas = {
    val newrow = canv.rows(point.row).updated(point.column, colour)
    Canvas(canv.rows.updated(point.row, newrow))
  }

  private def drawLine(startPos: Coordinates, endPos: Coordinates)(canvas: Canvas): (String \/ Canvas) = {
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

      drawLayer(ColourLayer(linePoints, lineColour), canvas)
    }
  }
}
