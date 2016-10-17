package edu.spbau.master.scala.course.task2

//Реалзуйте IntArrayBuffer с интерфейсом IntTraversable
trait IntTraversable {
  def isEmpty: Boolean

  def size: Int

  def contains(element: Int): Boolean

  def head: Int

  def tail: IntTraversable

  def ++(traversable: IntTraversable): IntTraversable

  def filter(predicate: Int => Boolean): IntTraversable

  def map(function: Int => Int): IntTraversable

  def flatMap(function: Int => IntTraversable): IntTraversable

  def foreach(function: Int => Unit): Unit
}

class IntArrayBuffer(private var _size: Int) extends IntTraversable {
  var array: Array[Int] = new Array[Int](_size)
  var _length = 0


  def apply(index: Int): Int = {
    checkIndex(index)
    array(index)
  }

  def update(index: Int, element: Int): Unit = {
    checkIndex(index)
    array(index) = element
  }

  private def checkIndex(index: Int): Unit =
    if (index >= size) {
      throw new NoSuchElementException
    }

  def clear(): Unit = {
    if (array.length > IntArrayBuffer.DefaultCapacity) {
      array = new Array[Int](IntArrayBuffer.DefaultCapacity)
    }
    _length = 0
  }

  def +=(element: Int): IntArrayBuffer = {
    ensureSize(size + 1)
    array(size) = element
    _length += 1
    this
  }

  def ++=(elements: IntTraversable): IntArrayBuffer = {
    ensureSize(size + elements.size)
    elements.foreach { e ⇒
      array(size) = e
      _length += 1
    }
    this
  }

  def remove(index: Int): Int = {
    checkIndex(index)
    val result = array(index)
    if (index != size - 1) {
      Array.copy(array, index + 1, array, index, size - (index + 1))
    }
    _length -= 1
    result
  }

  override def isEmpty: Boolean = size == 0

  override def size: Int = _length

  override def contains(element: Int): Boolean =
    (0 until size).exists(array(_) == element)

  override def head: Int = {
    checkIndex(0)
    array(0)
  }

  override def tail: IntArrayBuffer = {
    checkIndex(0)
    val tailArr = new IntArrayBuffer(array.length)
    Array.copy(array, 1, tailArr.array, 0, size - 1)
    tailArr._length = _length - 1
    tailArr
  }

  override def ++(traversable: IntTraversable): IntArrayBuffer = {
    copy ++= traversable
  }

  private def copy: IntArrayBuffer = {
    val copyArr = new IntArrayBuffer(array.length)
    Array.copy(array, 0, copyArr.array, 0, size)
    copyArr._length = size
    copyArr
  }

  protected def ensureSize(size: Int): Unit = {
    if (size > array.length) {
      var newLength = array.length * 2
      while (size > newLength) {
        newLength *= 2
      }
      val newArray = new Array[Int](newLength)
      Array.copy(array, 0, newArray, 0, array.length)
      array = newArray
    }
  }

  override def filter(predicate: (Int) => Boolean): IntTraversable = {
    val x = for {
      i ← 0 until size
      if predicate(array(i))
    } yield {
      array(i)
    }
    IntArrayBuffer(x: _*)
  }

  override def map(function: (Int) => Int): IntTraversable = {
    val c = new IntArrayBuffer(array.length)
    c._length = size
    for {
      i ← 0 until size
    } {
      c(i) = function(array(i))
    }
    c
  }

  override def flatMap(function: (Int) => IntTraversable): IntTraversable = {
    val arr = IntArrayBuffer.empty
    for {
      i ← 0 until size
    } {
      arr ++= function(array(i))
    }
    arr
  }

  override def foreach(function: (Int) => Unit): Unit = {
    for {
      i ← 0 until size
    } {
      function(array(i))
    }
  }
}

object IntArrayBuffer {
  private val DefaultCapacity = 16

  def empty: IntArrayBuffer = new IntArrayBuffer(DefaultCapacity)

  def apply(elements: Int*): IntArrayBuffer = {
    val x = new IntArrayBuffer(elements.size)
    elements.foreach(x += _)
    x
  }

  def unapplySeq(buffer: IntArrayBuffer): Option[Seq[Int]] = Some(for {
    i ← 0 until buffer.size
  } yield {
    buffer(i)
  })
} 