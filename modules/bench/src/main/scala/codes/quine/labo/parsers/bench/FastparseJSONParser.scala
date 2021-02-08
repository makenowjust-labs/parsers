package codes.quine.labo.parsers.bench

import fastparse.NoWhitespace._
import fastparse._

import scala.annotation.switch

import JSON._

object FastparseJSONParser {
  def space[_: P]: P[Unit] = CharsWhileIn(" \r\n", 0)
  def digits[_: P]: P[Unit] = CharsWhileIn("0123456789")
  def exponent[_: P]: P[Unit] = CharIn("eE") ~ CharIn("+\\-").? ~ digits
  def fractional[_: P]: P[Unit] = "." ~ digits
  def integral[_: P]: P[Unit] = "0" | CharIn("123456789") ~ digits.?

  def number[_: P]: P[JSON] =
    (CharIn("+\\-").? ~ integral ~ fractional.? ~ exponent.?).!.map(s => JSONNumber(s.toDouble)).opaque("<number>")

  def `null`[_: P]: P[JSON] = "null" ~ Pass(JSONNull)
  def `true`[_: P]: P[JSON] = "true" ~ Pass(JSONBoolean(true))
  def `false`[_: P]: P[JSON] = "false" ~ Pass(JSONBoolean(false))

  def hexDigit[_: P]: P[Unit] = CharIn("0123456789abcdefABCDEF")
  def unicodeEscape[_: P]: P[Char] = "u" ~ hexDigit.rep(exactly = 4).!.map(s => Integer.parseInt(s, 16).toChar)
  def simpleEscape[_: P]: P[Char] =
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
  def escape[_: P]: P[Char] = "\\" ~/ (simpleEscape | unicodeEscape)
  def stringContent[_: P]: P[String] = CharsWhile(c => c != '"' && c != '\\').! | escape.map(String.valueOf(_))
  def key[_: P]: P[String] = ("\"" ~/ stringContent.rep.map(_.mkString) ~ "\"").opaque("<string>")
  def string[_: P]: P[JSON] = key.map(JSONString(_))

  def arrayContent[_: P]: P[Seq[JSON]] =
    (json ~ ("," ~ space ~/ json).rep).?.map {
      case None          => Seq.empty
      case Some((x, xs)) => x +: xs
    }
  def array[_: P]: P[JSON] = "[" ~ space ~/ arrayContent.map(JSONArray(_)) ~ "]"

  def pair[_: P]: P[(String, JSON)] = key ~ space ~ ":" ~ space ~ json
  def objectContent[_: P]: P[Seq[(String, JSON)]] =
    (pair ~ ("," ~ space ~/ pair).rep).?.map {
      case None             => Seq.empty
      case Some((k, v, xs)) => (k, v) +: xs
    }
  def `object`[_: P]: P[JSON] = "{" ~ space ~/ objectContent.map(JSONObject(_)) ~ "}"

  def json[_: P]: P[JSON] =
    (number | string | `null` | `true` | `false` | array | `object`) ~ space

  def entry[_: P]: P[JSON] = space ~ json ~ End

  def parse(input: String): Parsed[JSON] =
    fastparse.parse(input, entry(_))
}
