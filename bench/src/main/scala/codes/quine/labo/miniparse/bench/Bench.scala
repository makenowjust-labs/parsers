package codes.quine.labo
package miniparse
package bench

import org.openjdk.jmh.annotations._

class Bench {
  @Benchmark
  def measureMiniparse(): miniparse.Parsed[JSON] =
    MiniparseJSONParser.parse(Bench.source)

  @Benchmark
  def measureFastparse(): fastparse.Parsed[JSON] =
    FastparseJSONParser.parse(Bench.source)

  @Benchmark
  def measureParserCombinators(): ParserCombinatorsJSONParser.ParseResult[JSON] =
    ParserCombinatorsJSONParser.parse(Bench.source)

  @Benchmark
  def measureAtto(): atto.ParseResult[JSON] =
    AttoJSONParser.parse(Bench.source)
}

object Bench {
  val source = scala.io.Source.fromInputStream(getClass.getResourceAsStream("/posts.json")).mkString
}
