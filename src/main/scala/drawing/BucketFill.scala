package drawing

import scala.collection.mutable
import scala.collection.immutable

object BucketFill {

  def buildLayer(origin: Coordinates, colour: Char, canvas: Canvas): ColourLayer = {
    // A choice of implementations...

    buildLayerWithRecursion(origin, colour, canvas)
    //buildLayerWithMutableStack(origin, colour, canvas)
  }

  private def buildLayerWithRecursion(origin: Coordinates, colour: Char, canvas: Canvas): ColourLayer = {
    type Set = immutable.HashSet[Coordinates]
    type Stack = immutable.Stack[Coordinates]

    def pushNode(layerPoints: Set)(nodeStack: Stack, node: Coordinates): Stack =
      if(canvas.getNode(node).isDefined && !layerPoints.contains(node))
        nodeStack.push(node)
      else
        nodeStack

    def fillAcc(layerPoints: Set, nodeStack: Stack): Set = {
      if (nodeStack.isEmpty) {
        layerPoints
      } else {
        val (node, poppedStack) = nodeStack.pop2
        val (newPoints, newStack) = if (canvas.getNode(node).get == emptyPosition) {
          val push = pushNode(layerPoints)_
          // TODO Can push be used as an infix operator?
          val tempStack = push(push(push(push(poppedStack, node.left), node.right), node.up), node.down)
          (layerPoints + node, tempStack)
        } else {
          (layerPoints, poppedStack)
        }
        fillAcc(newPoints, newStack)
      }
    }

    (fillAcc(new Set, new Stack().push(origin)).toSeq, colour)
  }

  private def buildLayerWithMutableStack(origin: Coordinates, colour: Char, canvas: Canvas): ColourLayer = {
    val layerPoints = new mutable.HashSet[Coordinates]
    val nodeStack = new mutable.Stack[Coordinates]

    def pushNode(node: Coordinates) =
      if(canvas.getNode(node).isDefined && !layerPoints.contains(node))
        nodeStack.push(node)

    pushNode(origin)

    while (!nodeStack.isEmpty) {

      val nodePosition = nodeStack.pop

      if (canvas.getNode(nodePosition).get == emptyPosition) {

        layerPoints.add(nodePosition)

        pushNode(nodePosition.left)
        pushNode(nodePosition.right)
        pushNode(nodePosition.up)
        pushNode(nodePosition.down)
      }
    }

    (layerPoints.toSeq, colour)
  }

}
