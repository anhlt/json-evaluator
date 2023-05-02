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
    Operator.BangEqualToken.parse | Operator.BangToken.parse

  // == | =
  val equalEqualOrEqual: P[Operator] =
    Operator.EqualEqualToken.parse | Operator.EqualToken.parse

  // >= | >
  val greaterEqualOrGreater: P[Operator] =
    Operator.GreaterEqualToken.parse | Operator.GreaterToken.parse

  // <= | <
  val lessEqualOrLess: P[Operator] =
    Operator.LessEqualToken.parse | Operator.LessToken.parse

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

  val intNumber: P[Literal] = N.digits.string.map(Literal.Number(_))

  val floatNumber: P[Literal] = {
    val fraction = (P.char('.') *> N.digits).string
    (N.digits ~ fraction).string.map(Literal.FloatNumber(_))
  }




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
      Operator.ArrowToken.parse,
      Operator.LeftParenToken.parse,
      Operator.RightParenToken.parse,
      Operator.LeftBraceToken.parse,
      Operator.RightBraceToken.parse,
      Operator.LeftBracketToken.parse,
      Operator.RightBracketToken.parse,
      Operator.CommaToken.parse,
      Operator.ColonToken.parse,
      Operator.DotToken.parse,
      Operator.MinusToken.parse,
      Operator.PlusToken.parse,
      Operator.SemicolonToken.parse,
      Operator.StarToken.parse,
      bangEqualOrBang,
      equalEqualOrEqual,
      greaterEqualOrGreater,
      lessEqualOrLess,
      comments,
      identifier,
      str,
      intNumber,
      floatNumber
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
