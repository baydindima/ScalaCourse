package edu.spbau.master.scala.course.task2

import org.scalatest.{FreeSpec, Matchers}

/**
  * Created by Dmitriy Baidin.
  */
class IntArrayBufferSpec extends FreeSpec with Matchers {

  "A IntArrayBuffer" - {
    "when empty" - {
      "should have size 0" in {
        IntArrayBuffer.empty.size shouldEqual 0
      }
      "should have isEmpty true" in {
        IntArrayBuffer.empty.isEmpty shouldEqual true
      }
      "should produce NoSuchElementException when head is invoked " in {
        assertThrows[NoSuchElementException] {
          IntArrayBuffer.empty.head
        }
      }
      "should produce NoSuchElementException when tail is invoked " in {
        assertThrows[NoSuchElementException] {
          IntArrayBuffer.empty.head
        }
      }
      "should return empty IntArrayBuffer when map invoked" in {
        IntArrayBuffer.empty.map(_ * 2).isEmpty shouldEqual true
      }
      "should return empty IntArrayBuffer when flatMap invoked" in {
        IntArrayBuffer.empty.flatMap(IntArrayBuffer(1, 2, 3, _)).isEmpty shouldEqual true
      }
      "should return empty IntArrayBuffer when filter invoked" in {
        IntArrayBuffer.empty.filter(_ ⇒ true).isEmpty shouldEqual true
      }
    }
    "should construct from seq of elements " in {
      IntArrayBuffer(1, 2, 3, 4, 5) match {
        case IntArrayBuffer(1, 2, 3, 4, 5) ⇒
        case _ ⇒ fail()
      }
    }
    "should return element value by index when apply invoked" in {
      val buf = IntArrayBuffer(1, 2, 3, 4, 5)
      buf(0) shouldEqual 1
      buf(1) shouldEqual 2
      buf(2) shouldEqual 3
      buf(3) shouldEqual 4
      buf(4) shouldEqual 5
    }
    "should throw NoSuchElementException when index too big" in {
      assertThrows[NoSuchElementException] {
        val buf = IntArrayBuffer(1, 2, 3, 4, 5)
        val x = buf(5)
      }
    }
    "should throw ArrayIndexOutOfBoundsException when index is negative" in {
      assertThrows[ArrayIndexOutOfBoundsException] {
        val buf = IntArrayBuffer(1, 2, 3, 4, 5)
        val x = buf(-1)
      }
    }
    "should update element value by index when update invoked" in {
      val buf = IntArrayBuffer(1, 2, 3, 4, 5)
      buf(0) = 42
      buf(0) shouldEqual 42
    }
    "should be empty after clear invoked" in {
      val buf = IntArrayBuffer(1, 2, 3, 4, 5)
      buf.clear()
      buf.isEmpty shouldEqual true
    }
    "should add new element when += invoked" in {
      val buf = IntArrayBuffer.empty
      for (i ← 0 until 1000) {
        buf += i
      }
      buf.size shouldEqual 1000
      buf(0) shouldEqual 0
      buf(500) shouldEqual 500
      buf(999) shouldEqual 999
    }
    "should add all new elements when  ++= invoked" in {
      val buf = IntArrayBuffer.empty
      for (i ← 0 until 1000) {
        buf += i
      }
      buf ++= buf
      buf.size shouldEqual 2000
      buf(0) shouldEqual 0
      buf(500) shouldEqual 500
      buf(999) shouldEqual 999
      buf(1000) shouldEqual 0
      buf(1500) shouldEqual 500
      buf(1999) shouldEqual 999
    }
    "should remove element" in {
      val buf = IntArrayBuffer.empty
      for (i ← 0 until 1000) {
        buf += i
      }
      buf.remove(999)
      buf.size shouldEqual 999
      buf.remove(0)
      buf.size shouldEqual 998
      buf(0) shouldEqual 1
      buf(997) shouldEqual 998
    }
    "should construct new IntArrayBuffer with additional elements when ++ invoked" in {
      val buf = IntArrayBuffer.empty
      for (i ← 0 until 1000) {
        buf += i
      }
      val buf2 = buf ++ buf
      buf.size shouldEqual 1000
      buf2.size shouldEqual 2000
      buf2(0) shouldEqual 0
      buf2(500) shouldEqual 500
      buf2(999) shouldEqual 999
      buf2(1000) shouldEqual 0
      buf2(1500) shouldEqual 500
      buf2(1999) shouldEqual 999
    }
    "when not empty" - {
      "should return false when isEmpty invoked" in {
        IntArrayBuffer(1).isEmpty shouldEqual false
      }
      "should first element when head invoked" in {
        IntArrayBuffer(1, 2, 3).head shouldEqual 1
      }
      "should return tail" in {
        IntArrayBuffer(1, 2, 3).tail match {
          case IntArrayBuffer(2, 3) ⇒
          case _ ⇒ fail
        }
      }
    }
    "should return only matching elements when filter invoked" in {
      IntArrayBuffer(1, 2, 3, 4, 5).filter(_ % 2 == 0) match {
        case IntArrayBuffer(2, 4) ⇒
        case _ ⇒ fail
      }
    }
    "should return IntArrayBuffer with updated elements when map invoked" in {
      IntArrayBuffer(1, 2, 3).map(_ * 2) match {
        case IntArrayBuffer(2, 4, 6) ⇒
        case _ ⇒ fail
      }
    }
    "should return IntArrayBuffer with updated elements when flatMap invoked" in {
      IntArrayBuffer(1, 2).flatMap(i ⇒ IntArrayBuffer(i * 2, i * 3)) match {
        case IntArrayBuffer(2, 3, 4, 6) ⇒
        case _ ⇒ fail
      }
    }
  }

}
