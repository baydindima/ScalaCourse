package edu.spbau.master.scala.course.task5

abstract class Graph {
  type Node <: NodeImpl

  abstract class NodeImpl {
    def connect(other: Node): Node
  }
  def isConnected(n1: Node, n2: Node): Boolean

  def newNode: Node
}


class DirectedGraph extends Graph {
  override type Node = DirectedGraphNode

  class DirectedGraphNode(val neighbors: List[Node]) extends NodeImpl {
    override def connect(other: Node): Node = new DirectedGraphNode(other :: neighbors)
  }

  override def isConnected(n1: Node, n2: Node): Boolean = n1.neighbors.contains(n2)

  override def newNode: Node = new DirectedGraphNode(List.empty)
}


class ColoredDirectedGraph extends Graph {
  override type Node = ColoredDirectedNode

  class ColoredDirectedNode(val color: Int, val neighbors: List[Node]) extends NodeImpl {
    def changeColor(newColor: Int): Node = new ColoredDirectedNode(newColor, neighbors)

    override def connect(other: Node): Node = new ColoredDirectedNode(color, other :: neighbors)
  }

  override def isConnected(n1: Node, n2: Node): Boolean = n1.neighbors.contains(n2)

  override def newNode: Node = new ColoredDirectedNode(0, List.empty)

}