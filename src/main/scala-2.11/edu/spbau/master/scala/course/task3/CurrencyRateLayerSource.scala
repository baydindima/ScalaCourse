package edu.spbau.master.scala.course.task3

import edu.spbau.master.scala.course.task3.CurrencyType.usd
import play.api.libs.json.{JsNumber, JsObject, JsValue, Json}

import scala.io.Source
import scala.util.Try

object CurrencyRateLayerSource extends CurrencyRateSource {
  implicit val currencyRateSource: CurrencyRateSource = this

  private val AccessKey = "16e577a8289cf3556ed725bba7296551"

  private val UsdRate: Double = 1

  override def convertFrom(from: CurrencyAmount,
                           to: CurrencyType,
                           maybeDate: Option[Date] = None): CurrencyAmount =
    from.currencyType match {
      case curType if curType == to => from
      case curType =>
        val currenciesRate: Map[CurrencyType, Double] = (maybeDate match {
          case Some(date) => getCurrencyToUsdRateHistorical(
            date,
            Seq(curType, to).filter(_ != usd)
          ).toMap
          case None => getCurrencyToUsdRateLive(
            Seq(curType, to).filter(_ != usd)
          ).toMap
        }) + (usd -> UsdRate)

        CurrencyAmount(from.amount * currenciesRate(to) / currenciesRate(curType), to)
    }

  private def getCurrencyToUsdRateHistorical(date: Date, currencies: Seq[CurrencyType]): Seq[(CurrencyType, Double)] = {
    request(s"http://apilayer.net/api/historical?" +
      s"access_key=$AccessKey&" +
      s"date=2005-02-01&" +
      s"currencies=${currencies.mkString(",")}&")
  }

  private def getCurrencyToUsdRateLive(currencies: Seq[CurrencyType]): Seq[(CurrencyType, Double)] = {
    request(s"http://apilayer.net/api/live?" +
      s"access_key=$AccessKey&" +
      s"currencies=${currencies.mkString(",")}&")
  }

  private def request(url: String): Seq[(CurrencyType, Double)] = Try {
    def invalidResponse(jsValue: JsValue): Nothing =
      throw new RuntimeException(s"Invalid response format: $jsValue")
    val jsValue = Json.parse(Source.fromURL(url, "ISO-8859-1").mkString)
    jsValue match {
      case JsObject(fields) => fields.get("quotes") match {
        case Some(JsObject(results)) => results
          .map {
            case (name, JsNumber(value)) =>
              (CurrencyType.fromString(name.substring(3)), value.toDouble)
            case _ => invalidResponse(jsValue)
          }.toSeq
        case _ => invalidResponse(jsValue)
      }
      case _ => invalidResponse(jsValue)
    }
  }.get
}