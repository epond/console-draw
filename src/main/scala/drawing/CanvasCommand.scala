package drawing

import scalaz._
import Scalaz._

sealed trait CanvasCommand
case class NewCanvasCommand(width: Int, height: Int) extends CanvasCommand
case class DrawLineCommand(startPos: Coordinates, endPos: Coordinates) extends CanvasCommand
case class DrawRectangleCommand(ulCorner: Coordinates, lrCorner: Coordinates) extends CanvasCommand
case class BucketFillCommand(origin: Coordinates, colour: Char) extends CanvasCommand