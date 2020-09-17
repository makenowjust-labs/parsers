# parsers

> Scala parser combinator libraries collection.

## About

This repository contains the following libraries:

  - [`parsers-common`](modules/parsers-common): Common utilities for parser combinator.
  - [`parsers-contparse`](modules/parsers-contparse): A parser combinator library with continuation passing style.
  - [`parsers-funcparse`](modules/parsers-funcparse): A basic functional parser combinator library.
  - [`parsers-inlineparse`](modules/parsers-inlineparse): A faster parser combinator library with mutable state & inline expansion.
  - [`parsers-stackparse`](modules/parsers-stackparse): A stackless parser combinator library.

## API

`P[T]` is parser type, `Parsed[T]` is result type, and the following method is parser executor:

```scala
def parse[T](input: String, p: P[T]): Parsed[T] = ...
```

You can create `P[T]` by the following primitives, and combinate parsers by the following combinators.

### Primitives

  - `"literal"`: just matches `"literal"`.
  - `CharIn("abc")`: matches a character contained in `"abc"`.
  - `CharPred(isXXX)`: matches a character with which `isXXX` returns `true`.
  - `CharsWhileIn("abc")` matches seqence of characters contained in `"abc"`.
  - `CharsWhile(isXXX)` matches sequence of characters with which `isXXX` returns `true`.
  - `AnyChar`: matches an any character.
  - `Start`: only matches at start position.
  - `End`: only matches at end position.
  - `p.!`: captures a string on `p` matching.

### Combinators

  - `P(p)`: same as `p`, but it is lazy evaluated (for recursive parsers).
  - `Pass`: succeeds with `()` value.
  - `Pass(v)`: succeeds with `v`.
  - `Fail`: fails matching.
  - `Fail(msg)`: fails matching with `msg`.
  - `&?(p)`: matches when `p` is matched, but is does not consume any characters (positive look-ahead).
  - `&!(p)`: matches when `p` is not matched, but is does not consume any characters (negative look-ahead).
  - `p1 ~ p2`: matches `p1` and then matches `p2` sequentially.
  - `p1 ~/ p2`: matches `p1`, cuts backtrack, and then matches `p2` sequentially.
  - `p./`: matches `p` and then cuts backtrack.
  - `p1 | p2`: matches `p1`, or matches `p2`.
  - `p.rep`: repeats `p` matching many times.
  - `p.rep(min)`: repeats `p` matching `n` times (`min <= n`).
  - `p.rep(min, max)`: repeats `p` matching `n` times (`min <= n <= max`).
  - `p.count(n)`: repeats `p` matching `n` times.
  - `p.?`: matches `p`, or succeeds without consuming any characters.
  - `p.named(name)`: names `p` for better error message.
  - `p.map(f), p.flatMap(f), p.filter(f)`: usual operators.

## License

MIT License.

2020 (C) TSUYUSATO "MakeNowJust" Kitsune
