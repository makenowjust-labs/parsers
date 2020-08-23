# 0.2.0 (2020-08-23)

Changes:

  - (parsers-common): Add `parsers-common`.
  - (parsers-inlineparse): Rename `miniparse` to `parsers-inlineparse`.
  - (parsers-inlineparse): Add `AnyChar` and `NoCut` operators.
  - (parsers-stackparse): Rename `stackparse` to `parsers-stackparse`.
  - (parsers-stackparse) Split common utilities into `parsers-common`.

Fixes:

  - (parsers-inlineparse): Fix some behavior same as stackparse.

# 0.1.1 (2020-08-17)

Changes:

  - (stackparse): Add `&?` (positive look-ahead) and `&!` (negative look-ahead) combinators.
  - (stackparse): Add no argument version of `Pass` and `Fail`.

Fixes:

  - (stackparse): Fix some bugs found by tests.

# 0.1.0 (2020-08-15)

Changes:

  - (miniparse): Add `miniparse`.
  - (stackparse): Add `stackparse`.
