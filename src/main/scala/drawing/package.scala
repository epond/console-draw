package object drawing {
  /** the positions at which the given 'colour' should be drawn */
  case class ColourLayer(points: Seq[Coordinates], colour: Char)

  val emptyPosition = ' '
  val lineColour = 'x'
  val emptyLayer = ColourLayer(Nil, emptyPosition)

  val isOutOfBounds: (Coordinates, Canvas) => Boolean = (point, canvas) => {
    (point.column < 1 || point.column > canvas.width) ||
    (point.row < 1    || point.row    > canvas.height)
  }
}
