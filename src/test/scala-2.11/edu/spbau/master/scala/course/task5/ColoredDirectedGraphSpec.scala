package edu.spbau.master.scala.course.task5

import org.scalatest.{FreeSpec, Matchers}


class ColoredDirectedGraphSpec  extends FreeSpec with Matchers {

  "A ColoredDirectedGraph" - {
    "should be empty after init" in {
      val g = new ColoredDirectedGraph
      g.nodes should have size 0
      g.edges should have size 0
    }

    "should return new node with color 0" in {
      val g = new ColoredDirectedGraph
      g.newNode.color shouldEqual 0
    }

    "should connect node in one way" in {
      val g = new ColoredDirectedGraph
      val node1 = g.newNode
      val node2 = g.newNode
      g.connect(node1, node2)
      g.isConnected(node1, node2) shouldBe true
      g.isConnected(node2, node1) shouldBe false
    }
  }

}
