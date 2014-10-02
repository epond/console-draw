package drawing

case class Canvas(rows: Vector[Vector[Char]]) {
  // height and width concern the drawable area inside margin characters
  val height = if (rows.size == 0) 0 else rows.size - 2
  val width = if (rows.size == 0) 0 else rows(0).size - 2

  override def toString: String = {
    val rowsWithLineBreaks = for {
      rowWithIndex <- rows.zipWithIndex
    } yield {
      val (row, rowIndex) = rowWithIndex
      if (rowIndex <= (height))
        row :+ '\n'
      else
        row
    }
    rowsWithLineBreaks.flatten.mkString
  }

  def getNode(position: Coordinates): Option[Char] = {
    if (!isOutOfBounds(position, this)) Some(rows(position.row)(position.column)) else None
  }
}

object Canvas {
  def apply(width: Int, height: Int): Canvas = {
    val edgeRow = Vector.fill(width+2)('-')
    val innerRow = '|' +: Vector.fill(width)(emptyPosition) :+ '|'
    Canvas(edgeRow +: Vector.fill(height)(innerRow) :+ edgeRow)
  }
  def apply(str: String): Canvas = {
    Canvas(str.split('\n').map(_.toVector).toVector)
  }
  val empty = Canvas(Vector.empty)
}

case class Coordinates(column: Int, row: Int) {
  def -(that: Coordinates): Coordinates = Coordinates(column - that.column, row - that.row)
  lazy val left =  Coordinates(column-1, row)
  lazy val right = Coordinates(column+1, row)
  lazy val up =    Coordinates(column,   row-1)
  lazy val down =  Coordinates(column,   row+1)
}