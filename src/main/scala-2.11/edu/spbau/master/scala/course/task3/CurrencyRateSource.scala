package edu.spbau.master.scala.course.task3

trait CurrencyRateSource {
  def convertFrom(from: CurrencyAmount, to: CurrencyType, maybeDate: Option[Date] = None): CurrencyAmount
}
