package codes.quine.labo.parsers.stackparse

import minitest.SimpleTestSuite

object ParserSuite extends SimpleTestSuite {
  def assertSuccess[T](s: String, parser: P[T], expected: T, pos: Int): Unit =
    assertEquals(parse(s, parser), Parsed.Success(expected, pos))

  def assertFailure(s: String, parser: P[Any], message: String, pos: Int): Unit =
    assertEquals(parse(s, parser), Parsed.Failure(message, pos))

  test("Literal") {
    assertSuccess("", "", (), 0)
    assertSuccess("a", "a", (), 1)
    assertFailure("b", "a", "expected: \"a\"", 0)
    assertSuccess("aa", "a", (), 1)
    assertSuccess("abc", "abc", (), 3)
    assertFailure("ab", "abc", "expected: \"abc\"", 0)
    assertSuccess("あい", "あい", (), 2)
    assertFailure("うえ", "あい", "expected: \"あい\"", 0)
    assertSuccess("あいうえお", "あい", (), 2)
  }

  test("CharIn") {
    assertSuccess("a", CharIn("ab"), (), 1)
    assertSuccess("aa", CharIn("ab"), (), 1)
    assertSuccess("b", CharIn("ab"), (), 1)
    assertFailure("", CharIn("ab"), "expected: CharIn(\"ab\")", 0)
    assertFailure("c", CharIn("ab"), "expected: CharIn(\"ab\")", 0)
  }

  test("CharPred") {
    assertSuccess("a", CharPred(_.isLower), (), 1)
    assertSuccess("aa", CharPred(_.isLower), (), 1)
    assertSuccess("b", CharPred(_.isLower), (), 1)
    assertFailure("", CharPred(_.isLower), "expected: CharPred(...)", 0)
    assertFailure("A", CharPred(_.isLower), "expected: CharPred(...)", 0)
  }

  test("CharsWhileIn") {
    assertSuccess("a", CharsWhileIn("abc"), (), 1)
    assertSuccess("abcba", CharsWhileIn("abc"), (), 5)
    assertSuccess("abcdcba", CharsWhileIn("abc"), (), 3)
    assertSuccess("", CharsWhileIn("abc", 0), (), 0)
    assertFailure("", CharsWhileIn("abc"), "expected: CharsWhileIn(\"abc\")", 0)
    assertFailure("def", CharsWhileIn("abc"), "expected: CharsWhileIn(\"abc\")", 0)
  }

  test("CharsWhile") {
    assertSuccess("a", CharsWhile(_.isLower), (), 1)
    assertSuccess("abcba", CharsWhile(_.isLower), (), 5)
    assertSuccess("abcAcba", CharsWhile(_.isLower), (), 3)
    assertSuccess("", CharsWhile(_.isLower, 0), (), 0)
    assertFailure("", CharsWhile(_.isLower), "expected: CharsWhile(...)", 0)
    assertFailure("ABC", CharsWhile(_.isLower), "expected: CharsWhile(...)", 0)
  }

  test("AnyChar") {
    assertSuccess("a", AnyChar, (), 1)
    assertSuccess("aa", AnyChar, (), 1)
    assertSuccess("A", AnyChar, (), 1)
    assertFailure("", AnyChar, "expected: AnyChar", 0)
  }

  test("Start") {
    assertSuccess("", Start, (), 0)
    assertSuccess("a", Start, (), 0)
    assertFailure("a", "a" ~ Start, "expected: Start", 1)
  }

  test("End") {
    assertSuccess("", End, (), 0)
    assertSuccess("a", "a" ~ End, (), 1)
    assertFailure("a", End, "expected: End", 0)
  }

  test("&?") {
    assertEquals(&?(AnyChar).toString, "&?(AnyChar)")
    assertSuccess("a", &?("a"), (), 0)
    assertSuccess("a", &?("a".!), "a", 0)
    assertFailure("a", &?(AnyChar./) ~ "b" | "a", "expected: \"b\"", 0)
  }

  test("&!") {
    assertEquals(&!(AnyChar).toString, "&!(AnyChar)")
    assertSuccess("", &!(AnyChar), (), 0)
    assertFailure("a", &!(AnyChar), "unexpected: AnyChar", 0)
    assertFailure("ab", &!(AnyChar ~ "a" | "ab"), "unexpected: AnyChar ~ \"a\" | \"ab\"", 0)
    assertFailure("ab", &!(AnyChar ~/ "a") ~ "aa" | "ab", "expected: \"aa\"", 0)
  }

  test("Parser#!") {
    assertEquals(AnyChar.!.toString, "AnyChar.!")
    assertSuccess("abc", "abc".!, "abc", 3)
    assertSuccess("abc", "abc".!.!, "abc", 3)
    assertSuccess("aaa", CharsWhileIn("a").!, "aaa", 3)
    assertSuccess("aaab", CharsWhileIn("a").!, "aaa", 3)
    assertFailure("", "abc".!, "expected: \"abc\"", 0)
  }

  test("Parser#~") {
    assertEquals((AnyChar ~ AnyChar).toString, "AnyChar ~ AnyChar")
    assertEquals((AnyChar ~ AnyChar ~ AnyChar).toString, "AnyChar ~ AnyChar ~ AnyChar")
    assertEquals((AnyChar ~ (AnyChar | AnyChar)).toString, "AnyChar ~ (AnyChar | AnyChar)")
    assertSuccess("ab", "a" ~ "b", (), 2)
    assertSuccess("ab", "a".! ~ "b", "a", 2)
    assertSuccess("ab", "a" ~ "b".!, "b", 2)
    assertSuccess("ab", "a".! ~ "b".!, ("a", "b"), 2)
    assertSuccess("abc", "a".! ~ "b".! ~ "c".!, ("a", "b", "c"), 3)
    assertSuccess("abcd", "a".! ~ "b".! ~ "c".! ~ "d".!, ("a", "b", "c", "d"), 4)
    assertSuccess("abcd", "" ~ "a".! ~ "b".! ~ "" ~ "c".! ~ "d".! ~ "", ("a", "b", "c", "d"), 4)
    assertSuccess("ab", ("a" ~ "b").!, "ab", 2)
    assertFailure("bc", "a" ~ "b", "expected: \"a\"", 0)
    assertFailure("ac", "a" ~ "b", "expected: \"b\"", 1)
  }

  test("Parser#rep") {
    assertEquals(AnyChar.rep.toString, "AnyChar.rep")
    assertEquals(AnyChar.rep(1).toString, "AnyChar.rep(1)")
    assertEquals(AnyChar.rep(1, 2).toString, "AnyChar.rep(1, 2)")
    assertEquals((AnyChar ~ AnyChar).rep.toString, "(AnyChar ~ AnyChar).rep")
    assertSuccess("", "a".rep, (), 0)
    assertSuccess("aaa", "a".rep, (), 3)
    assertSuccess("aaa", "a".rep.!, "aaa", 3)
    assertSuccess("aaa", "a".!.rep, Seq("a", "a", "a"), 3)
    assertSuccess("aab", "a".rep, (), 2)
    assertSuccess("aaab", ("a" ~ "a").rep, (), 2)
    assertFailure("aaab", ("a" ~/ "a").rep, "expected: \"a\"", 3)
    assertSuccess("", ("a" | "").rep, (), 0)
    assertSuccess("a", ("a" | "").rep, (), 1)
    assertFailure("", ("a" | ""./).rep, "expected: \"a\"", 0)
    assertFailure("a", ("a" | ""./).rep, "expected: \"a\"", 1)
    assertSuccess("ba", ("a" | "b"./).rep, (), 2)
    assertFailure("ba", ("a" | "b"./).rep ~ "c" | "ba", "expected: \"a\", \"b\", \"c\"", 2)
    assertSuccess("aa", "a".rep(2), (), 2)
    assertFailure("", "a".rep(2), "expected: \"a\"", 0)
    assertFailure("a", "a".rep(2), "expected: \"a\"", 1)
    assertSuccess("a", "a".rep(0, 1), (), 1)
    assertSuccess("aa", "a".rep(0, 1), (), 1)
  }

  test("Parser#count") {
    assertEquals(AnyChar.count(3).toString, "AnyChar.count(3)")
    assertEquals((AnyChar ~ AnyChar).count(3).toString, "(AnyChar ~ AnyChar).count(3)")
    assertSuccess("aa", "a".count(2), (), 2)
    assertSuccess("aa", "a".count(2).!, "aa", 2)
    assertSuccess("aa", "a".!.count(2), Seq("a", "a"), 2)
    assertFailure("", "a".count(2), "expected: \"a\"", 0)
    assertFailure("a", "a".count(2), "expected: \"a\"", 1)
  }

  test("Parser#?") {
    assertEquals(AnyChar.?.toString, "AnyChar.?")
    assertEquals((AnyChar ~ AnyChar).?.toString, "(AnyChar ~ AnyChar).?")
    assertSuccess("", "a".?, (), 0)
    assertSuccess("a", "a".?, (), 1)
    assertSuccess("aa", "a".?, (), 1)
    assertSuccess("", "a".!.?, None, 0)
    assertSuccess("a", "a".!.?, Some("a"), 1)
    assertSuccess("a", ("a" ~ "b").?, (), 0)
    assertFailure("a", ("a" ~/ "b").?, "expected: \"b\"", 1)
  }

  test("Parser#|") {
    assertEquals((AnyChar | AnyChar).toString, "AnyChar | AnyChar")
    assertEquals((AnyChar | AnyChar | AnyChar).toString, "AnyChar | AnyChar | AnyChar")
    assertEquals((AnyChar | AnyChar ~ AnyChar).toString, "AnyChar | AnyChar ~ AnyChar")
    assertSuccess("a", "a" | "b", (), 1)
    assertSuccess("b", "a" | "b", (), 1)
    assertSuccess("a", "a".! | "b".!, "a", 1)
    assertSuccess("b", "a".! | "b".!, "b", 1)
    assertFailure("c", "a" | "b", "expected: \"a\", \"b\"", 0)
    assertFailure("az", "a" ~ "b" | "b" ~ "c", "expected: \"b\"", 1)
    assertFailure("az", "b" ~ "c" | "a" ~ "b", "expected: \"b\"", 1)
    assertSuccess("ac", "a" ~ "b" | "ac", (), 2)
    assertFailure("ac", "a" ~/ "b" | "ac", "expected: \"b\"", 1)
    assertFailure("abc", ("a" ~/ "b" | "ac") ~ "d" | "abc", "expected: \"d\"", 2)
    assertFailure("abc", ("ac" | "a" ~/ "b") ~ "d" | "abc", "expected: \"d\"", 2)
  }

  test("Parser#map") {
    assertEquals(AnyChar.map(_ => 1).toString, "AnyChar.map(...)")
    assertEquals((AnyChar ~ AnyChar).map(_ => 1).toString, "(AnyChar ~ AnyChar).map(...)")
    assertSuccess("a", AnyChar.map(_ => 1), 1, 1)
    assertSuccess("a", AnyChar.!.map(_.length), 1, 1)
    assertFailure("b", "a".!.map(_.length), "expected: \"a\"", 0)
  }

  test("Parser#flatMap") {
    assertEquals(AnyChar.flatMap(_ => Pass(1)).toString, "AnyChar.flatMap(...)")
    assertEquals((AnyChar ~ AnyChar).flatMap(_ => Pass(1)).toString, "(AnyChar ~ AnyChar).flatMap(...)")
    assertSuccess("a", AnyChar.flatMap(_ => Pass(1)), 1, 1)
    assertSuccess("ab", AnyChar.flatMap(_ => AnyChar), (), 2)
    assertSuccess("aa", AnyChar.!.flatMap(s => s), (), 2)
    assertFailure("ab", AnyChar.!.flatMap(s => s), "expected: \"a\"", 1)
    assertFailure("ab", AnyChar./.flatMap(_ => "a" | "c") | "ab", "expected: \"a\", \"c\"", 1)
  }

  test("Parser#filter") {
    assertEquals(AnyChar.filter(_ => true).toString, "AnyChar.filter(...)")
    assertEquals((AnyChar ~ AnyChar).filter(_ => true).toString, "(AnyChar ~ AnyChar).filter(...)")
    assertSuccess("a", AnyChar.!.filter(_ == "a"), "a", 1)
    assertFailure("b", AnyChar.!.filter(_ == "a"), "filter", 0)
  }

  test("P") {
    lazy val p: P[Unit] = P("a" ~ "b" | "c" ~ p)
    assertEquals(p.toString, "p")
    assertSuccess("ab", p, (), 2)
    assertSuccess("cab", p, (), 3)
    assertFailure("ac", p, "expected: \"b\"", 1)
  }

  test("Parser#named") {
    assertEquals(AnyChar.named("foo").toString, "AnyChar.named(\"foo\")")
    assertSuccess("ab", ("a" ~ "b").named("foo"), (), 2)
    assertFailure("aa", ("a" ~ "b").named("foo"), "expected: foo", 0)
    assertFailure("abc", ("a" ~ "b").named("foo") ~ "a", "expected: \"a\"", 2)
  }

  test("Parser#/") {
    assertEquals(AnyChar./.toString, "AnyChar./")
    assertEquals((AnyChar ~ AnyChar)./.toString, "(AnyChar ~ AnyChar)./")
    assertFailure("aa", AnyChar./ ~ "b" | "aa", "expected: \"b\"", 1)
  }

  test("Pass") {
    assertEquals(Pass.toString, "Pass")
    assertEquals(Pass(0).toString, "Pass(0)")
    assertSuccess("", Pass(0), 0, 0)
    assertSuccess("a", Pass(0), 0, 0)
    assertSuccess("", Pass, (), 0)
  }

  test("Fail") {
    assertEquals(Fail.toString, "Fail")
    assertEquals(Fail("foo").toString, "Fail(\"foo\")")
    assertFailure("", Fail, "fail", 0)
    assertFailure("", Fail("foo"), "foo", 0)
  }
}
