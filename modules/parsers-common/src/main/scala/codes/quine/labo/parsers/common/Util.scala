package codes.quine.labo.parsers.common

object Util {
  def escape(s: String): String =
    s.toList.map {
      case '\b'                           => "\\b"
      case '\f'                           => "\\f"
      case '\n'                           => "\\n"
      case '\r'                           => "\\r"
      case '\t'                           => "\\t"
      case '\"'                           => "\\\""
      case '\\'                           => "\\\\"
      case c if Character.isISOControl(c) => f"\\x${c}%02x"
      case c                              => c.toString
    }.mkString
}
