package drawing

import scalaz._
import org.specs2.mutable.Specification

class CanvasCommandSpec extends Specification {
  "A DrawLine command" should {
    "accept only horizontal or vertical lines" in {
      DrawLine.build(Coordinates(1,1), Coordinates(1,2)) match {
        case \/-(command) => {
          command.startPos must beEqualTo(Coordinates(1,1))
          command.endPos must beEqualTo(Coordinates(1,2))
        }
        case -\/(error)   => ko
      }
      DrawLine.build(Coordinates(1,2), Coordinates(3,2)) match {
        case \/-(command) => {
          command.startPos must beEqualTo(Coordinates(1,2))
          command.endPos must beEqualTo(Coordinates(3,2))
        }
        case -\/(error)   => ko
      }
      DrawLine.build(Coordinates(1,1), Coordinates(3,2)) match {
        case \/-(_)     => ko("Expected error was not returned")
        case -\/(error) => error must beEqualTo("Only horizontal and vertical lines are supported")
      }
    }
  }
}
