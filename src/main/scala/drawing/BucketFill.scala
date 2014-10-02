package drawing

import scala.collection.mutable

object BucketFill {

  def buildLayer(origin: Coordinates, colour: Char, canvas: Canvas): ColourLayer = {
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
