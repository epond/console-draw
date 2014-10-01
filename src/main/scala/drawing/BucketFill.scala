package drawing

import scala.collection.mutable

object BucketFill {

  def buildLayer(origin: Coordinates, colour: Char, canvas: Canvas): ColourLayer = {
    val layerPoints = new mutable.HashSet[Coordinates]
    val nodeStack = new mutable.Stack[Coordinates]

    nodeStack.push(origin)

    while (!nodeStack.isEmpty) {
      val nodePosition = nodeStack.pop
      if (canvas.getNode(nodePosition).get == emptyPosition) {

        layerPoints.add(nodePosition)

        if(canvas.getNode(nodePosition.left).isDefined && !layerPoints.contains(nodePosition.left))
          nodeStack.push(nodePosition.left)

        if(canvas.getNode(nodePosition.right).isDefined && !layerPoints.contains(nodePosition.right))
          nodeStack.push(nodePosition.right)

        if(canvas.getNode(nodePosition.up).isDefined && !layerPoints.contains(nodePosition.up))
          nodeStack.push(nodePosition.up)

        if(canvas.getNode(nodePosition.down).isDefined && !layerPoints.contains(nodePosition.down))
          nodeStack.push(nodePosition.down)
      }
    }

    (layerPoints.toSeq, colour)
  }

}
