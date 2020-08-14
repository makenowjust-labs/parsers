# scala-labo-bench

> Scala benchmarks.

## Benchmarks

### parser

It compares parser combinator libraries speed by JSON parsing.

Comparison libraries:

  - [atto](http://tpolecat.github.io/atto/)
  - [fastparse](https://www.lihaoyi.com/fastparse/)
  - [miniparse](https://github.com/MakeNowJust-Labo/scala-labo-miniparse)
  - [parser-combinators](https://github.com/scala/scala-parser-combinators)
  - [stackparse](https://github.com/MakeNowJust-Labo/scala-labo-stackparse)

<details>
  <summary>Result (2020/08/15)</summary>

```
Benchmark                        Mode  Cnt     Score     Error  Units
Bench.measureAtto               thrpt    3    90.321 ±   4.673  ops/s
Bench.measureFastparse          thrpt    3  2996.322 ± 217.350  ops/s
Bench.measureMiniparse          thrpt    3  2424.982 ± 203.496  ops/s
Bench.measureParserCombinators  thrpt    3   329.207 ±  54.708  ops/s
Bench.measureStackparse         thrpt    3   538.673 ± 211.648  ops/s
```

</details>

## License

[CC-0 1.0](https://creativecommons.org/publicdomain/zero/1.0/)

2020 (C) TSUYUSATO "MakeNowJust" Kitsune
