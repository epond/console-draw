package drawing

import scalaz._
import Scalaz._

sealed trait CanvasCommand
case class NewCanvas(width: Int, height: Int) extends CanvasCommand
case class DrawLine(startPos: Coordinates, endPos: Coordinates) extends CanvasCommand
case class DrawRectangle(ulCorner: Coordinates, lrCorner: Coordinates) extends CanvasCommand
case class BucketFill(origin: Coordinates, colour: Char) extends CanvasCommand

// Use the companion objects to construct commands to enforce validation
object DrawLine {
  def build(startPos: Coordinates, endPos: Coordinates): (String \/ DrawLine) = {
    val difference = endPos - startPos
    if (difference.row != 0 && difference.column != 0) {
      -\/("Only horizontal and vertical lines are supported")
    } else {
      \/-(DrawLine(startPos, endPos))
    }
  }
}