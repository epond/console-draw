package drawing

case class Canvas(rows: Vector[Vector[Char]]) {
  override def toString: String = {
    val rowsWithLineBreaks = for {
      rowWithIndex <- rows.zipWithIndex
    } yield {
      val (row, rowIndex) = rowWithIndex
      if (rowIndex < (rows.size-1))
        row :+ '\n'
      else
        row
    }
    rowsWithLineBreaks.flatten.mkString
  }
}

object Canvas {
  def apply(str: String): Canvas = {
    Canvas(str.split('\n').map(_.toVector).toVector)
  }
}