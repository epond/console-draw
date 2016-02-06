package drawing

import bettereither._

object DrawingConsole extends App {
  var running = true
  var canvas = Canvas.empty

  while(running) {
    val enteredCommand = readLine("enter command: ")
    if (enteredCommand.equalsIgnoreCase("q")) {
      running = false;
    } else {
      CommandInterpreter.parseCommand(enteredCommand) match {
        case Some(command) => {
          DrawingEngine.applyCommand(command)(canvas) match {
            case \/-(newCanvas) => {
              canvas = newCanvas
              println(canvas + "\n")
            }
            case -\/(error) => println(error)
          }
        }
        case None => {
          println("Command not recognised")
        }
      }
    }
  }
}
