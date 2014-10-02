package drawing

object CommandInterpreter {

  val separatorChar = ' '

  def parseCommand(enteredCommand: String): Option[CanvasCommand] = {
    val commandSplit = enteredCommand.split(separatorChar)
    // Command letter is case-insensitive
    val commandArray = commandSplit.updated(0, commandSplit(0).toUpperCase)
    if (commandArray.length > 0) {
      try {
        commandArray match {
          case Array("C", w, h) => {
            Some(NewCanvasCommand(w.toInt, h.toInt))
          }
          case Array("L", startCol, startRow, endCol, endRow) => {
            Some(DrawLineCommand(
              Coordinates(startCol.toInt, startRow.toInt),
              Coordinates(endCol.toInt, endRow.toInt)))
          }
          case Array("R", ulCol, ulRow, lrCol, lrRow) => {
            Some(DrawRectangleCommand(
              Coordinates(ulCol.toInt, ulRow.toInt),
              Coordinates(lrCol.toInt, lrRow.toInt)))
          }
          case Array("B", originCol, originRow, colour) => {
            Some(BucketFillCommand(
              Coordinates(originCol.toInt, originRow.toInt),
              colour.charAt(0)))
          }
          case _ => None
        }
      } catch {
        case _: NumberFormatException => None
      }
    } else None
  }

}
