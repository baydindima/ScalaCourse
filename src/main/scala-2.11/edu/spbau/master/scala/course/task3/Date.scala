package edu.spbau.master.scala.course.task3

class Date(day: Int, month: Int, year: Int) {
  override def toString: String = s"$day-$month-$year"
}


object DateConversions {

  class DayMonth(day: Int, month: Int) {
    def --(year: Int): Date = new Date(day, month, year)
  }

  implicit class Day(day: Int) {
    def --(month: Int): DayMonth = new DayMonth(day, month)
  }

}

