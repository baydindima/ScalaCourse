package edu.spbau.master.scala.course.task5

import org.scalatest.{FreeSpec, Matchers}


class ColoredDirectedGraphSpec  extends FreeSpec with Matchers {

  "A ColoredDirectedGraph" - {
    "should return new node with empty neighbors" in {
      val g = new ColoredDirectedGraph
      g.newNode.neighbors should have size 0
    }

    "should return new node with color 0" in {
      val g = new ColoredDirectedGraph
      g.newNode.color shouldEqual 0
    }

    "should connect node in one way" in {
      val g = new ColoredDirectedGraph
      val node1 = g.newNode
      val node2 = g.newNode
      g.isConnected(node1.connect(node2), node2) shouldBe true
      g.isConnected(node1, node2) shouldBe false
      g.isConnected(node2, node1.connect(node2)) shouldBe false
    }

    "should change color" in {
      val g = new ColoredDirectedGraph
      g.newNode.changeColor(10).color shouldEqual 10
    }
  }

}
