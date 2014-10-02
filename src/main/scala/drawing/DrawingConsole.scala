package drawing

object DrawingConsole extends App {
  var running = true
  var canvas = Canvas.empty

  while(running) {
    var enteredCommand = readLine("enter command: ")
    if (enteredCommand.equalsIgnoreCase("q")) {
      running = false;
    } else {
      CommandInterpreter.parseCommand(enteredCommand) match {
        case Some(command) => {
          println(command) // TODO
        }
        case None => {
          println("Command not recognised")
        }
      }
    }
  }
}
