package edu.spbau.master.scala.course.task5

import scala.collection.mutable.ArrayBuffer

abstract class Graph {
  type Node <: NodeImpl
  type Edge <: EdgeImpl

  trait NodeImpl {}

  trait EdgeImpl {
    def from: Node

    def to: Node
  }

  def connect(from: Node, to: Node): Edge

  def isConnected(from: Node, to: Node): Boolean = edges.exists(e => e.to == to && e.from == from)

  def newNode: Node

  def edges: List[Edge]

  def nodes: List[Node]
}


class DirectedGraph extends Graph {
  override type Node = DirectedGraphNode
  override type Edge = DirectedGraphEdge

  class DirectedGraphEdge private(val from: Node, val to: Node) extends EdgeImpl

  private object DirectedGraphEdge {
    def apply(from: Node, to: Node): Edge = new DirectedGraphEdge(from, to)
  }

  class DirectedGraphNode private() extends NodeImpl {}

  private object DirectedGraphNode {
    def apply(): Node = new DirectedGraphNode()
  }

  override def newNode: Node = {
    val newNode = DirectedGraphNode()
    nodesArray += newNode
    newNode
  }

  override def connect(from: Node, to: Node): Edge = {
    val edge = DirectedGraphEdge(from, to)
    edgesArray += edge
    edge
  }

  override def edges: List[DirectedGraphEdge] = edgesArray.toList

  override def nodes: List[DirectedGraphNode] = nodesArray.toList

  private val edgesArray: ArrayBuffer[Edge] = ArrayBuffer.empty
  private val nodesArray: ArrayBuffer[Node] = ArrayBuffer.empty
}


class ColoredDirectedGraph extends Graph {
  override type Node = ColoredDirectedGraphNode
  override type Edge = ColoredDirectedGraphEdge

  class ColoredDirectedGraphEdge private(val from: Node, val to: Node) extends EdgeImpl

  private object ColoredDirectedGraphEdge {
    def apply(from: Node, to: Node): Edge = new ColoredDirectedGraphEdge(from, to)
  }

  class ColoredDirectedGraphNode private(var color: Int = 0) extends NodeImpl {}

  private object ColoredDirectedGraphNode {
    def apply(): Node = new ColoredDirectedGraphNode()
  }

  override def newNode: Node = {
    val newNode = ColoredDirectedGraphNode()
    nodesArray += newNode
    newNode
  }

  override def connect(from: Node, to: Node): Edge = {
    val edge = ColoredDirectedGraphEdge(from, to)
    edgesArray += edge
    edge
  }

  override def edges: List[ColoredDirectedGraphEdge] = edgesArray.toList

  override def nodes: List[ColoredDirectedGraphNode] = nodesArray.toList

  private val edgesArray: ArrayBuffer[Edge] = ArrayBuffer.empty
  private val nodesArray: ArrayBuffer[Node] = ArrayBuffer.empty
}