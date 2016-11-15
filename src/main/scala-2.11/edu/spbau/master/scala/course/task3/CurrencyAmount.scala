package edu.spbau.master.scala.course.task3

import edu.spbau.master.scala.course.task3.CurrencyConversions.CurrencyRequest

sealed trait CurrencyType

object CurrencyType {
  private val stringToCurrency: Map[String, CurrencyType] = Map(
    rub.toString -> rub,
    eur.toString -> eur,
    usd.toString -> usd
  )

  def fromString(string: String) = stringToCurrency(string)

  object rub extends CurrencyType {
    override def toString: String = "RUB"
  }

  object eur extends CurrencyType {
    override def toString: String = "EUR"
  }

  object usd extends CurrencyType {
    override def toString: String = "USD"
  }

}


case class CurrencyAmount(amount: Double, currencyType: CurrencyType) {
  def to(toType: CurrencyType): CurrencyRequest = {
    new CurrencyRequest(this, toType)
  }
}