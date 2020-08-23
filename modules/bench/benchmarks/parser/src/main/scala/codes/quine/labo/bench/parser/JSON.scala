package codes.quine.labo.bench.parser

trait JSON

object JSON {
  final case class JSONObject(value: Seq[(String, JSON)]) extends JSON
  final case class JSONArray(value: Seq[JSON]) extends JSON
  final case class JSONString(value: String) extends JSON
  final case class JSONNumber(value: Double) extends JSON
  final case class JSONBoolean(value: scala.Boolean) extends JSON
  case object JSONNull extends JSON
}
