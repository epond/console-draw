package drawing

import org.specs2.mutable.Specification

class CommandInterpreterSpec extends Specification {
  "A CommandInterpreter" should {
    "parse new canvas" in {
      CommandInterpreter.parseCommand("C 20 4") must beEqualTo(
        Some(NewCanvasCommand(20, 4))
      )
    }

    "parse draw line" in {
      CommandInterpreter.parseCommand("L 1 2 6 2") must beEqualTo(
        Some(DrawLineCommand(Coordinates(1, 2), Coordinates(6, 2)))
      )
    }

    "parse draw rectangle" in {
      CommandInterpreter.parseCommand("R 16 1 20 3") must beEqualTo(
        Some(DrawRectangleCommand(Coordinates(16, 1), Coordinates(20, 3)))
      )
    }

    "parse bucket fill" in {
      CommandInterpreter.parseCommand("B 10 3 o") must beEqualTo(
        Some(BucketFillCommand(Coordinates(10, 3), 'o'))
      )
    }

    "parse clear" in {
      CommandInterpreter.parseCommand("CLR") must beEqualTo(
        Some(ClearCommand)
      )
    }

    "handle unrecognised command" in {
      CommandInterpreter.parseCommand("unknown") must beNone
    }

    "handle malformed input" in {
      CommandInterpreter.parseCommand("C 20") must beNone
      CommandInterpreter.parseCommand("C 20 x") must beNone
      CommandInterpreter.parseCommand("L 1 2 6") must beNone
      CommandInterpreter.parseCommand("L 1 2 6 x") must beNone
      CommandInterpreter.parseCommand("R 16 1 20") must beNone
      CommandInterpreter.parseCommand("R 16 1 20 x") must beNone
      CommandInterpreter.parseCommand("B 10 2") must beNone
      CommandInterpreter.parseCommand("B 10 x o") must beNone
    }
  }
}
