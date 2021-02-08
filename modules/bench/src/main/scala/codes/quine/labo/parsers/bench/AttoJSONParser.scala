package codes.quine.labo.parsers.bench

import atto.Atto._
import atto._

import scala.annotation.switch

import JSON._

object AttoJSONParser {
  def number: Parser[JSON] = double.map(JSONNumber(_)).named("<number>")

  def `null`: Parser[JSON] = Atto.string("null") ~> ok(JSONNull)
  def `true`: Parser[JSON] = Atto.string("true") ~> ok(JSONBoolean(true))
  def `false`: Parser[JSON] = Atto.string("false") ~> ok(JSONBoolean(false))

  def hexDigit: Parser[Char] = oneOf("0123456789abcdefABCDEF")
  def unicodeEscape: Parser[Char] =
    char('u') ~> count(4, hexDigit).map { case ds => Integer.parseInt(ds.mkString, 16).toChar }
  def simpleEscape: Parser[Char] =
    oneOf("\"\\/bfnrt").map(c =>
      (c: @switch) match {
        case 'b' => '\b'
        case 'f' => '\f'
        case 'n' => '\n'
        case 'r' => '\r'
        case 't' => '\t'
        case c   => c
      }
    )
  def escape: Parser[Char] = char('\\') ~> (simpleEscape | unicodeEscape)
  def stringContent: Parser[String] = takeWhile1(c => c != '"' && c != '\\') | escape.map(String.valueOf(_))
  def key: Parser[String] = (char('"') ~> many(stringContent).map(_.mkString) <~ char('"')).named("<string>")
  def string: Parser[JSON] = key.map(JSONString(_))

  def arrayContent: Parser[Seq[JSON]] =
    opt(json ~ many(char(',') ~> skipWhitespace ~> json)).map {
      case None          => Seq.empty
      case Some((x, xs)) => x +: xs
    }
  def array: Parser[JSON] = char('[') ~> skipWhitespace ~> arrayContent.map(JSONArray(_)) <~ char(']')

  def pair: Parser[(String, JSON)] = key ~ (skipWhitespace ~> char(':') ~> skipWhitespace ~> json)
  def objectContent: Parser[Seq[(String, JSON)]] =
    opt(pair ~ many(char(',') ~> skipWhitespace ~> pair)).map {
      case None          => Seq.empty
      case Some((x, xs)) => x +: xs
    }
  def `object`: Parser[JSON] = char('{') ~> skipWhitespace ~> objectContent.map(JSONObject(_)) <~ char('}')

  def json: Parser[JSON] =
    (number | string | `null` | `true` | `false` | array | `object`) <~ skipWhitespace

  def entry: Parser[JSON] = skipWhitespace ~> json <~ endOfInput

  def parse(input: String): ParseResult[JSON] =
    entry.parseOnly(input)
}
