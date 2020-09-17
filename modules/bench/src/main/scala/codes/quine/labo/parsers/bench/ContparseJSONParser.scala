package codes.quine.labo.parsers
package bench

import scala.annotation.switch

import contparse._
import JSON._

object ContparseJSONParser {
  val space: P[Unit] = CharsWhileIn(" \r\n", 0)

  val json: P[JSON] =
    P((number | string | `null` | `true` | `false` | array | `object`) ~ space)

  val entry: P[JSON] = space ~ json ~ End

  val digits: P[Unit] = CharsWhileIn("0123456789")
  val exponent: P[Unit] = CharIn("eE") ~ CharIn("+-").? ~ digits
  val fractional: P[Unit] = "." ~ digits
  val integral: P[Unit] = "0" | CharIn("123456789") ~ digits.?

  val number: P[JSON] =
    (CharIn("+-").? ~ integral ~ fractional.? ~ exponent.?).!.map(s => JSONNumber(s.toDouble)).named("<number>")

  val `null`: P[JSON] = "null" ~ Pass(JSONNull)
  val `true`: P[JSON] = "true" ~ Pass(JSONBoolean(true))
  val `false`: P[JSON] = "false" ~ Pass(JSONBoolean(false))

  val hexDigit: P[Unit] = CharIn("0123456789abcdefABCDEF")
  val unicodeEscape: P[Char] = "u" ~ hexDigit.count(4).!.map(s => Integer.parseInt(s, 16).toChar)
  val simpleEscape: P[Char] =
    CharIn("\"\\/bfnrt").!.map(s =>
      (s.charAt(0): @switch) match {
        case 'b' => '\b'
        case 'f' => '\f'
        case 'n' => '\n'
        case 'r' => '\r'
        case 't' => '\t'
        case c   => c
      }
    )
  val escape: P[Char] = "\\" ~/ (simpleEscape | unicodeEscape)
  val stringContent: P[String] = CharsWhile(c => c != '"' && c != '\\').! | escape.map(String.valueOf(_))
  val key: P[String] = ("\"" ~/ stringContent.rep.map(_.mkString) ~ "\"").named("<string>")
  val string: P[JSON] = key.map(JSONString(_))

  val arrayContent: P[Seq[JSON]] =
    (json ~ ("," ~ space ~/ json).rep).?.map {
      case None          => Seq.empty
      case Some((x, xs)) => x +: xs
    }
  val array: P[JSON] = "[" ~ space ~/ arrayContent.map(JSONArray(_)) ~ "]"

  val pair: P[(String, JSON)] = key ~ space ~ ":" ~ space ~ json
  val objectContent: P[Seq[(String, JSON)]] =
    (pair ~ ("," ~ space ~/ pair).rep).?.map {
      case None             => Seq.empty
      case Some((k, v, xs)) => (k, v) +: xs
    }
  val `object`: P[JSON] = "{" ~ space ~/ objectContent.map(JSONObject(_)) ~ "}"

  def parse(input: String): Parsed[JSON] =
    contparse.parse(input, entry)
}
