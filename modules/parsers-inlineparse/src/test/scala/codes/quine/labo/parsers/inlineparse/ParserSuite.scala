package codes.quine.labo.parsers.inlineparse

import minitest.SimpleTestSuite

object ParserSuite extends SimpleTestSuite {
  def assertSuccess[T](s: String, expected: T, pos: Int)(parser: P[_] => P[T]): Unit =
    assertEquals(parse(s, parser), Parsed.Success(expected, pos))

  def assertFailure(s: String, message: String, pos: Int)(parser: P[_] => P[Any]): Unit =
    assertEquals(parse(s, parser), Parsed.Failure(message, pos))

  test("Literal") {
    assertSuccess("", (), 0)(implicit ctx => "")
    assertSuccess("a", (), 1)(implicit ctx => "a")
    assertFailure("b", "expected: \"a\"", 0)(implicit ctx => "a")
    assertSuccess("aa", (), 1)(implicit ctx => "a")
    assertSuccess("abc", (), 3)(implicit ctx => "abc")
    assertFailure("ab", "expected: \"abc\"", 0)(implicit ctx => "abc")
    assertSuccess("あい", (), 2)(implicit ctx => "あい")
    assertFailure("うえ", "expected: \"あい\"", 0)(implicit ctx => "あい")
    assertSuccess("あいうえお", (), 2)(implicit ctx => "あい")
  }

  test("CharIn") {
    assertSuccess("a", (), 1)(implicit ctx => CharIn("ab"))
    assertSuccess("aa", (), 1)(implicit ctx => CharIn("ab"))
    assertSuccess("b", (), 1)(implicit ctx => CharIn("ab"))
    assertFailure("", "expected: CharIn(\"ab\")", 0)(implicit ctx => CharIn("ab"))
    assertFailure("c", "expected: CharIn(\"ab\")", 0)(implicit ctx => CharIn("ab"))
  }

  test("CharPred") {
    assertSuccess("a", (), 1)(implicit ctx => CharPred(_.isLower))
    assertSuccess("aa", (), 1)(implicit ctx => CharPred(_.isLower))
    assertSuccess("b", (), 1)(implicit ctx => CharPred(_.isLower))
    assertFailure("", "expected: CharPred(...)", 0)(implicit ctx => CharPred(_.isLower))
    assertFailure("A", "expected: CharPred(...)", 0)(implicit ctx => CharPred(_.isLower))
  }

  test("CharsWhileIn") {
    assertSuccess("a", (), 1)(implicit ctx => CharsWhileIn("abc"))
    assertSuccess("abcba", (), 5)(implicit ctx => CharsWhileIn("abc"))
    assertSuccess("abcdcba", (), 3)(implicit ctx => CharsWhileIn("abc"))
    assertSuccess("", (), 0)(implicit ctx => CharsWhileIn("abc", 0))
    assertFailure("", "expected: CharsWhileIn(\"abc\")", 0)(implicit ctx => CharsWhileIn("abc"))
    assertFailure("def", "expected: CharsWhileIn(\"abc\")", 0)(implicit ctx => CharsWhileIn("abc"))
  }

  test("CharsWhile") {
    assertSuccess("a", (), 1)(implicit ctx => CharsWhile(_.isLower))
    assertSuccess("abcba", (), 5)(implicit ctx => CharsWhile(_.isLower))
    assertSuccess("abcAcba", (), 3)(implicit ctx => CharsWhile(_.isLower))
    assertSuccess("", (), 0)(implicit ctx => CharsWhile(_.isLower, 0))
    assertFailure("", "expected: CharsWhile(...)", 0)(implicit ctx => CharsWhile(_.isLower))
    assertFailure("ABC", "expected: CharsWhile(...)", 0)(implicit ctx => CharsWhile(_.isLower))
  }

  test("AnyChar") {
    assertSuccess("a", (), 1)(implicit ctx => AnyChar)
    assertSuccess("aa", (), 1)(implicit ctx => AnyChar)
    assertSuccess("A", (), 1)(implicit ctx => AnyChar)
    assertFailure("", "expected: AnyChar", 0)(implicit ctx => AnyChar)
  }

  test("Start") {
    assertSuccess("", (), 0)(implicit ctx => Start)
    assertSuccess("a", (), 0)(implicit ctx => Start)
    assertFailure("a", "expected: Start", 1)(implicit ctx => "a" ~ Start)
  }

  test("End") {
    assertSuccess("", (), 0)(implicit ctx => End)
    assertSuccess("a", (), 1)(implicit ctx => "a" ~ End)
    assertFailure("a", "expected: End", 0)(implicit ctx => End)
  }

  test("NoCut") {
    assertSuccess("a", (), 1)(implicit ctx => NoCut(AnyChar))
    assertSuccess("aa", (), 2)(implicit ctx => NoCut("a" ~/ "b") | "aa")
  }

  test("&?") {
    assertSuccess("a", (), 0)(implicit ctx => &?("a"))
    assertSuccess("a", "a", 0)(implicit ctx => &?("a".!))
    assertFailure("a", "expected: \"b\"", 0)(implicit ctx => &?(AnyChar./) ~ "b" | "a")
  }

  test("&!") {
    assertSuccess("", (), 0)(implicit ctx => &!(AnyChar))
    assertFailure("a", "negative look-ahead", 0)(implicit ctx => &!(AnyChar))
    assertFailure("ab", "negative look-ahead", 0)(implicit ctx => &!(AnyChar ~ "a" | "ab"))
    assertFailure("ab", "expected: \"aa\"", 0)(implicit ctx => &!(AnyChar ~/ "a") ~ "aa" | "ab")
  }

  test("Parser#!") {
    assertSuccess("abc", "abc", 3)(implicit ctx => "abc".!)
    assertSuccess("abc", "abc", 3)(implicit ctx => "abc".!.!)
    assertSuccess("aaa", "aaa", 3)(implicit ctx => CharsWhileIn("a").!)
    assertSuccess("aaab", "aaa", 3)(implicit ctx => CharsWhileIn("a").!)
    assertFailure("", "expected: \"abc\"", 0)(implicit ctx => "abc".!)
  }

  test("Parser#~") {
    assertSuccess("ab", (), 2)(implicit ctx => "a" ~ "b")
    assertSuccess("ab", "a", 2)(implicit ctx => "a".! ~ "b")
    assertSuccess("ab", "b", 2)(implicit ctx => "a" ~ "b".!)
    assertSuccess("ab", ("a", "b"), 2)(implicit ctx => "a".! ~ "b".!)
    assertSuccess("abc", ("a", "b", "c"), 3)(implicit ctx => "a".! ~ "b".! ~ "c".!)
    assertSuccess("abcd", ("a", "b", "c", "d"), 4)(implicit ctx => "a".! ~ "b".! ~ "c".! ~ "d".!)
    assertSuccess("abcd", ("a", "b", "c", "d"), 4)(implicit ctx => "" ~ "a".! ~ "b".! ~ "" ~ "c".! ~ "d".! ~ "")
    assertSuccess("ab", "ab", 2)(implicit ctx => ("a" ~ "b").!)
    assertFailure("bc", "expected: \"a\"", 0)(implicit ctx => "a" ~ "b")
    assertFailure("ac", "expected: \"b\"", 1)(implicit ctx => "a" ~ "b")
  }

  test("Parser#rep") {
    assertSuccess("", (), 0)(implicit ctx => "a".rep)
    assertSuccess("aaa", (), 3)(implicit ctx => "a".rep)
    assertSuccess("aaa", "aaa", 3)(implicit ctx => "a".rep.!)
    assertSuccess("aaa", Seq("a", "a", "a"), 3)(implicit ctx => "a".!.rep)
    assertSuccess("aab", (), 2)(implicit ctx => "a".rep)
    assertSuccess("aaab", (), 2)(implicit ctx => ("a" ~ "a").rep)
    assertFailure("aaab", "expected: \"a\"", 3)(implicit ctx => ("a" ~/ "a").rep)
    assertSuccess("", (), 0)(implicit ctx => ("a" | "").rep)
    assertSuccess("a", (), 1)(implicit ctx => ("a" | "").rep)
    assertFailure("", "expected: \"a\"", 0)(implicit ctx => ("a" | ""./).rep)
    assertFailure("a", "expected: \"a\"", 1)(implicit ctx => ("a" | ""./).rep)
    assertSuccess("ba", (), 2)(implicit ctx => ("a" | "b"./).rep)
    assertFailure("ba", "expected: \"a\", \"b\", \"c\"", 2)(implicit ctx => ("a" | "b"./).rep ~ "c" | "ba")
    assertSuccess("aa", (), 2)(implicit ctx => "a".rep(2))
    assertFailure("", "expected: \"a\"", 0)(implicit ctx => "a".rep(2))
    assertFailure("a", "expected: \"a\"", 1)(implicit ctx => "a".rep(2))
    assertSuccess("a", (), 1)(implicit ctx => "a".rep(0, 1))
    assertSuccess("aa", (), 1)(implicit ctx => "a".rep(0, 1))
  }

  test("Parser#count") {
    assertSuccess("aa", (), 2)(implicit ctx => ("a": P[Unit]).count(2))
    assertSuccess("aa", "aa", 2)(implicit ctx => ("a": P[Unit]).count(2).!)
    assertSuccess("aa", Seq("a", "a"), 2)(implicit ctx => "a".!.count(2))
    assertFailure("", "expected: \"a\"", 0)(implicit ctx => ("a": P[Unit]).count(2))
    assertFailure("a", "expected: \"a\"", 1)(implicit ctx => ("a": P[Unit]).count(2))
  }

  test("Parser#?") {
    assertSuccess("", (), 0)(implicit ctx => "a".?)
    assertSuccess("a", (), 1)(implicit ctx => "a".?)
    assertSuccess("aa", (), 1)(implicit ctx => "a".?)
    assertSuccess("", Option.empty[String], 0)(implicit ctx => "a".!.?)
    assertSuccess("a", Option("a"), 1)(implicit ctx => "a".!.?)
    assertSuccess("a", (), 0)(implicit ctx => ("a" ~ "b").?)
    assertFailure("a", "expected: \"b\"", 1)(implicit ctx => ("a" ~/ "b").?)
  }

  test("Parser#|") {
    assertSuccess("a", (), 1)(implicit ctx => "a" | "b")
    assertSuccess("b", (), 1)(implicit ctx => "a" | "b")
    assertSuccess("a", "a", 1)(implicit ctx => "a".! | "b".!)
    assertSuccess("b", "b", 1)(implicit ctx => "a".! | "b".!)
    assertFailure("c", "expected: \"a\", \"b\"", 0)(implicit ctx => "a" | "b")
    assertFailure("az", "expected: \"b\"", 1)(implicit ctx => "a" ~ "b" | "b" ~ "c")
    assertFailure("az", "expected: \"b\"", 1)(implicit ctx => "b" ~ "c" | "a" ~ "b")
    assertSuccess("ac", (), 2)(implicit ctx => "a" ~ "b" | "ac")
    assertFailure("ac", "expected: \"b\"", 1)(implicit ctx => "a" ~/ "b" | "ac")
    assertFailure("abc", "expected: \"d\"", 2)(implicit ctx => ("a" ~/ "b" | "ac") ~ "d" | "abc")
    assertFailure("abc", "expected: \"d\"", 2)(implicit ctx => ("ac" | "a" ~/ "b") ~ "d" | "abc")
  }

  test("Parser#map") {
    assertSuccess("a", 1, 1)(implicit ctx => AnyChar.map(_ => 1))
    assertSuccess("a", 1, 1)(implicit ctx => AnyChar.!.map(_.length))
    assertFailure("b", "expected: \"a\"", 0)(implicit ctx => "a".!.map(_.length))
  }

  test("Parser#flatMap") {
    assertSuccess("a", 1, 1)(implicit ctx => AnyChar.flatMap(_ => Pass(1)))
    assertSuccess("ab", (), 2)(implicit ctx => AnyChar.flatMap(_ => AnyChar))
    assertSuccess("aa", (), 2)(implicit ctx => AnyChar.!.flatMap(s => s))
    assertFailure("ab", "expected: StringLiteral(...)", 1)(implicit ctx => AnyChar.!.flatMap(s => s))
    assertFailure("ab", "expected: \"a\", \"c\"", 1)(implicit ctx => AnyChar./.flatMap(_ => "a" | "c") | "ab")
  }

  test("Parser#filter") {
    assertSuccess("a", "a", 1)(implicit ctx => AnyChar.!.filter(_ == "a"))
    assertFailure("b", "filter", 0)(implicit ctx => AnyChar.!.filter(_ == "a"))
  }

  test("P") {
    def p[_: P]: P[Unit] = P("a" ~ "b" | "c" ~ p)
    assertSuccess("ab", (), 2)(implicit ctx => p)
    assertSuccess("cab", (), 3)(implicit ctx => p)
    assertFailure("ac", "expected: \"b\"", 1)(implicit ctx => p)
  }

  test("Parser#named") {
    assertSuccess("ab", (), 2)(implicit ctx => ("a" ~ "b").named("foo"))
    assertFailure("aa", "expected: foo", 0)(implicit ctx => ("a" ~ "b").named("foo"))
    assertFailure("abc", "expected: \"a\"", 2)(implicit ctx => ("a" ~ "b").named("foo") ~ "a")
  }

  test("Parser#/") {
    assertFailure("aa", "expected: \"b\"", 1)(implicit ctx => AnyChar./ ~ "b" | "aa")
  }

  test("Pass") {
    assertSuccess("", 0, 0)(implicit ctx => Pass(0))
    assertSuccess("a", 0, 0)(implicit ctx => Pass(0))
    assertSuccess("", (), 0)(implicit ctx => Pass)
  }

  test("Fail") {
    assertFailure("", "fail", 0)(implicit ctx => Fail)
    assertFailure("", "foo", 0)(implicit ctx => Fail("foo"))
  }
}
