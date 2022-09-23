package io.jseval

import cats.*
import cats.data.NonEmptyList
import cats.parse.{
  Parser as P,
  Parser0 as P0,
  Rfc5234 as R,
  Numbers as N,
  LocationMap
}

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

  val singleLineComment: P[Comment] = {
    val start = P.string("--")
    val line: P0[String] = P.until0(endOfLine)
    (start *> line).string.map(Comment.SingleLine(_))
  }

  val blockComment: P[Comment] =
    val start = P.string("/*")
    val end = P.string("*/")
    val notStartOrEnd: P[Char] = (!(start | end)).with1 *> P.anyChar
    P.recursive[Comment.Block] { recurse =>
      (start *>
        (notStartOrEnd | recurse).rep0
        <* end).string.map(Comment.Block(_))
    }

  val comments: P[Token] = blockComment | singleLineComment

  val allTokens =
    keywords ++ List(
      Operator.Arrow.parse,
      Operator.LeftParen.parse,
      Operator.RightParen.parse,
      Operator.LeftBrace.parse,
      Operator.RightBrace.parse,
      Operator.LeftBracket.parse,
      Operator.RightBracket.parse,
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
      comments,
      identifier,
      str,
      number
    )

  val token: P[Token] = P.oneOf(allTokens).surroundedBy(whitespaces)

  val parser = token.rep.map(_.toList)

  def parse(str: String): Either[Error, List[Token]] = {
    val lm = LocationMap(str)
    parser.parse(str) match {
      case Right(("", ls)) => Right(ls)
      case Right((rest, ls)) =>
        val idx = str.indexOf(rest)
        Left(Error.PartialParse(ls, idx, lm))
      case Left(err) =>
        val idx = err.failedAtOffset
        Left(Error.ParseFailure(idx, lm))
    }
  }

  enum Error {
    case PartialParse[A](got: A, position: Int, locations: LocationMap)
        extends Error
    case ParseFailure(position: Int, locations: LocationMap) extends Error
  }

  extension (o: Operator) def parse = P.string(o.lexeme).as(o)
  extension (k: Keyword)
    def parse =
      (P.string(k.lexeme) ~ (whitespace | P.not(R.alpha) | P.end)).backtrack
        .as(k)

}
