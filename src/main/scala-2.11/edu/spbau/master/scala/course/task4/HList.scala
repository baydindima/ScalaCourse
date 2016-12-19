package edu.spbau.master.scala.course.task4

import scala.language.{implicitConversions, postfixOps}

case class HCons[+Head, +Tail <: HList](head: Head, tail: Tail) extends HList {
  override def toString: String = s"$head :: $tail"
}

object HNil extends HList {
  override def toString: String = "HNil"
}

sealed trait HList {
  def ::[T](v: T): HCons[T, this.type] = HCons(v, this)
}

object HList {
  type ::[+H, +T <: HList] = HCons[H, T]

  type HNil = HNil.type
  type ChurchZero = ChurchZero.type

  trait Splittable[T <: HList, C <: ChurchInt, L <: HList, R <: HList] {
    def apply(list: T, at: C): (L, R)
  }

  object Splittable {
    implicit def base[T <: HList]: Splittable[T, ChurchZero, HNil, T] =
      new Splittable[T, ChurchZero, HNil, T] {
        override def apply(list: T, at: ChurchZero): (HNil, T) = (HNil, list)
      }

    implicit def step[H, C <: ChurchInt, T <: HList, L <: HList, R <: HList]
    (implicit splittable: Splittable[T, C, L, R]): Splittable[H :: T, ChurchSuc[C], H :: L, R] = new Splittable[H :: T, ChurchSuc[C], H :: L, R] {
      override def apply(list: H :: T, at: ChurchSuc[C]): (H :: L, R) = {
        val (l, r) = splittable(list.tail, at.base)
        val nl = list.head :: l
        (nl, r)
      }
    }
  }

  def splitAt[T <: HList, C <: ChurchInt, L <: HList, R <: HList](source: T, at: C)(implicit splittable: Splittable[T, C, L, R]) = splittable(source, at)

  case class ChurchSuc[T <: ChurchInt](base: T) extends ChurchInt

  object ChurchZero extends ChurchInt

  sealed trait ChurchInt {
    def ++ = ChurchSuc(this)
  }

//  implicit def toChurchInt(n: Int): ChurchInt = n match {
//    case i if i <= 0 => ChurchZero
//    case _ =>
//      ChurchSuc(toChurchInt(n - 1))
//  }
}


object Test extends App {

  import HList._
  import Splittable._

  println("hello")
  val x = 10 :: "prev" :: HNil
  val y = HNil
  println(x)

  println(splitAt(y, ChurchZero))
  println(splitAt(x, ChurchSuc(ChurchZero)))
  //  println(splitAt(x, 1))
  //  println(splitAt(y, ChurchZero))

}
