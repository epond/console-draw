package drawing

import scala.annotation.tailrec
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

    @tailrec
    def buildLayerIter(layerPoints: Set, nodeStack: Stack): Set = {
      if (nodeStack.isEmpty) {
        layerPoints
      } else {
        val (node, poppedStack) = nodeStack.pop2
        val (newPoints, newStack) = if (canvas.getNode(node).get == emptyPosition) {
          val tempStack = NodeStack(canvas, poppedStack, layerPoints) push
            node.left push node.right push node.up push node.down
          (layerPoints + node, tempStack.stack)
        } else {
          (layerPoints, poppedStack)
        }
        buildLayerIter(newPoints, newStack)
      }
    }

    val initialPoints = new Set
    val initialStack = new Stack().push(origin)
    val layerPoints = buildLayerIter(initialPoints, initialStack).toSeq
    ColourLayer(layerPoints, colour)
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

    ColourLayer(layerPoints.toSeq, colour)
  }

}

case class NodeStack(canvas: Canvas, stack: immutable.Stack[Coordinates], layerPoints: immutable.HashSet[Coordinates]) {
  def push(node: Coordinates): NodeStack = {
    val newStack = if(canvas.getNode(node).isDefined && !layerPoints.contains(node))
      stack.push(node)
    else
      stack

    NodeStack(canvas, newStack, layerPoints)
  }
}
