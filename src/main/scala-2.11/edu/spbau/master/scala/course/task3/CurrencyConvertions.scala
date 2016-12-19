package edu.spbau.master.scala.course.task3

import edu.spbau.master.scala.course.task3.CurrencyConversions._
import edu.spbau.master.scala.course.task3.CurrencyRateLayerSource._
import edu.spbau.master.scala.course.task3.CurrencyType._
import edu.spbau.master.scala.course.task3.DateConversions._

import scala.language.postfixOps



object CurrencyConversions {

  implicit def toCurrencyAmount(currencyRequest: CurrencyRequest)(implicit
                                                                  currencyRateSource: CurrencyRateSource): CurrencyAmount = {
    import currencyRequest._
    currencyRateSource.convertFrom(amount, to, maybeDate)
  }

  class CurrencyRequest(val amount: CurrencyAmount, val to: CurrencyType, var maybeDate: Option[Date] = None) {
    def on(date: Date): CurrencyRequest = {
      maybeDate = Some(date)
      this
    }
  }

  implicit class CurrencyDouble(amount: Double) {
    def convert(currencyType: CurrencyType) = CurrencyAmount(amount, currencyType)

    def rub: CurrencyAmount = convert(CurrencyType.rub)

    def eur: CurrencyAmount = convert(CurrencyType.eur)

    def usd: CurrencyAmount = convert(CurrencyType.usd)
  }

}
object Test extends App {
  println(10.0.rub to eur on 21--11--2015: CurrencyAmount)
  println((10.0.rub to eur on 21--11--2015: CurrencyAmount) to rub on 21--11--2015: CurrencyAmount)
}
