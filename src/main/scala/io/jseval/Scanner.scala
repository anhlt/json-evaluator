package io.jseval

import cats.*
import cats.data.NonEmptyList
import cats.parse.{Parser as P, Parser0 as P0, Rfc5234 as R, Numbers as N}

object Scanner {

  val endOfLine: P[Unit] = R.cr | R.lf
  val whitespace: P[Unit] = endOfLine | R.wsp
  val whitespaces: P0[Unit] = P.until0(!whitespace).void

  // != | !
  val bangEqualOrBang: P[Operator] =
    Operator.BangEqual.parse | Operator.Bang.parse

  // == | =
  val equalEqualOrEqual: P[Operator] =
    Operator.EqualEqual.parse | Operator.Equal.parse

  // >= | >
  val greaterEqualOrGreater: P[Operator] =
    Operator.GreaterEqual.parse | Operator.Greater.parse

  // <= | <
  val lessEqualOrLess: P[Operator] =
    Operator.LessEqual.parse | Operator.Less.parse

  val keywords: List[P[Keyword]] = Keyword.values.map(_.parse).toList

  val identifier: P[Literal] = {
    val alphaOrUnderscore = R.alpha | P.char('_')
    val alphaNumeric = alphaOrUnderscore | N.digit

    (alphaOrUnderscore ~ alphaNumeric.rep0).string
      .map(Literal.Identifier(_))
  }

  val str: P[Literal] =
    P.until0(R.dquote).with1.surroundedBy(R.dquote).map(Literal.Str(_))

  // valid numbers: 1234 or 12.43
  // invalid numbers: .1234 or 1234.
  val number: P[Literal] = {
    val fraction = (P.char('.') *> N.digits).string.backtrack
    (N.digits ~ fraction.?).string.map(Literal.Number(_))
  }

  val allTokens =
    keywords ++ List(
      Operator.LeftParen.parse,
      Operator.RightParen.parse,
      Operator.LeftBrace.parse,
      Operator.RightBrace.parse,
      Operator.Comma.parse,
      Operator.Dot.parse,
      Operator.Minus.parse,
      Operator.Plus.parse,
      Operator.Semicolon.parse,
      Operator.Star.parse,
      bangEqualOrBang,
      equalEqualOrEqual,
      greaterEqualOrGreater,
      lessEqualOrLess,
      identifier,
      str,
      number
    )

  val token: P[Token] = P.oneOf(allTokens).surroundedBy(whitespaces)

  val parser = token.rep.map(_.toList)

  extension (o: Operator) def parse = P.string(o.lexeme).as(o)
  extension (k: Keyword)
    def parse = (P.string(k.lexeme) ~ (whitespace | P.end)).backtrack.as(k)

}
