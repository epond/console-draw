package drawing

import scalaz._
import Scalaz._

sealed trait CanvasCommand
case class NewCanvasCommand(width: Int, height: Int) extends CanvasCommand
case class DrawLineCommand(startPos: Coordinates, endPos: Coordinates) extends CanvasCommand
case class DrawRectangleCommand(ulCorner: Coordinates, lrCorner: Coordinates) extends CanvasCommand
case class BucketFillCommand(origin: Coordinates, colour: Char) extends CanvasCommand

// Use the companion objects to construct commands to enforce validation
object DrawLineCommand {
  def build(startPos: Coordinates, endPos: Coordinates): (String \/ DrawLineCommand) = {
    val difference = endPos - startPos
    if (difference.row != 0 && difference.column != 0) {
      -\/("Only horizontal and vertical lines are supported")
    } else {
      \/-(DrawLineCommand(startPos, endPos))
    }
  }
}