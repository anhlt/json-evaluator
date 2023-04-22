package io.jseval.parser

import io.jseval.Token
import cats._
import cats.implicits._
import io.jseval.CompilerError
import io.jseval.TypModule.Typ

object TypeParser extends BaseParser[Typ] {


  override val baseGrammar: BaseGrammar[Typ] = TypeGrammar

  override def constructOutPut(
      expr: Typ,
      rmn: List[Token]
  ): ParserResult[Typ] = TypeParserResult(expr, rmn)

  def parseType[F[_]](
      tokens: List[Token],
      precedence: Precendence = Precendence.LOWEST
  )(implicit
      a: MonadError[F, CompilerError]
  ): F[ParserResult[Typ]] =
    parsePrecedence(precedence, tokens)

}
