package codes.quine.labo.bench.parser

import scala.annotation.switch
import scala.util.parsing.combinator._

import JSON._

object ParserCombinatorsJSONParser extends RegexParsers {
  def space: Parser[Unit] = "[ \r\n]*".r ~> success(())
  def digits: Parser[String] = "[0123456789]+".r
  def exponent: Parser[String] = ("[eE]".r ~ "[+\\-]?".r ~ digits).map { case e ~ op ~ n => e + op + n }
  def fractional: Parser[String] = ("." ~ digits).map { case dot ~ n => dot + n }
  def integral: Parser[String] = "0" | ("[123456789]".r ~ digits.?).map { case d ~ n => d + n.getOrElse("") }

  def number: Parser[JSON] =
    ("[+\\-]?".r ~ integral ~ fractional.? ~ exponent.?)
      .map { case op ~ i ~ f ~ e => JSONNumber((op + i + f.getOrElse("") + e.getOrElse("")).toDouble) }
      .named("<number>")

  def `null`: Parser[JSON] = "null" ~> success(JSONNull)
  def `true`: Parser[JSON] = "true" ~> success(JSONBoolean(true))
  def `false`: Parser[JSON] = "false" ~> success(JSONBoolean(false))

  def hexDigit: Parser[String] = "[0123456789abcdefABCDEF]".r
  def unicodeEscape: Parser[Char] =
    "u" ~> (hexDigit ~ hexDigit ~ hexDigit ~ hexDigit).map {
      case d1 ~ d2 ~ d3 ~ d4 => Integer.parseInt(d1 + d2 + d3 + d4, 16).toChar
    }
  def simpleEscape: Parser[Char] =
    "[\"\\/bfnrt]".r.map(s =>
      (s.charAt(0): @switch) match {
        case 'b' => '\b'
        case 'f' => '\f'
        case 'n' => '\n'
        case 'r' => '\r'
        case 't' => '\t'
        case c   => c
      }
    )
  def escape: Parser[Char] = "\\" ~> (simpleEscape | unicodeEscape)
  def stringContent: Parser[String] = "[^\\\\\"]+".r | escape.map(String.valueOf(_))
  def key: Parser[String] = ("\"" ~> stringContent.*.map(_.mkString) <~ "\"").named("<string>")
  def string: Parser[JSON] = key.map(JSONString(_))

  def arrayContent: Parser[Seq[JSON]] =
    (json ~ ("," ~> space ~> json).*).?.map {
      case None         => Seq.empty
      case Some(x ~ xs) => x +: xs
    }
  def array: Parser[JSON] = "[" ~> space ~> arrayContent.map(JSONArray(_)) <~ "]"

  def pair: Parser[(String, JSON)] = (key ~ (space ~> ":" ~> space ~> json)).map { case k ~ v => (k, v) }
  def objectContent: Parser[Seq[(String, JSON)]] =
    (pair ~ ("," ~> space ~> pair).*).?.map {
      case None         => Seq.empty
      case Some(x ~ xs) => x +: xs
    }
  def `object`: Parser[JSON] = "{" ~> space ~> objectContent.map(JSONObject(_)) <~ "}"

  def json: Parser[JSON] =
    (number | string | `null` | `true` | `false` | array | `object`) <~ space

  def entry: Parser[JSON] = space ~> json <~ not(".".r)

  def parse(input: String): ParseResult[JSON] =
    parse(entry, input)
}
