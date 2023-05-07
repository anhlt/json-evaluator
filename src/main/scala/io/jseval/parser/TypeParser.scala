package io.jseval.parser

import io.jseval.Token
import cats._
import cats.implicits._
import io.jseval.CompilerError
import io.jseval.TypModule.Typ

object TypeParser extends BaseParser[Typ] {

  override val baseGrammar: BaseGrammar[Typ] = TypeGrammar

  override def recursiveInfix[F[_]](
      left: Typ,
      tokens: List[Token],
      minPrecenden: Precendence
  )(implicit a: MonadError[F, CompilerError]): F[ParserResult[Typ]] = {
    // op : lookahead
    tokens match {
      case op :: rest => {
        for {
          opPrecedence <- baseGrammar.getPrecedence(tokens) // op precedence
          parserOut <-
            if (minPrecenden.code < opPrecedence.code) {
              for {
                infixParser <- baseGrammar.mInfixParsers(tokens)

                // left int
                // opt ->
                newLeft <- infixParser.parse(
                  rest,
                  left
                ) // parse right, and return result
                // newLeft = TArrow(left, right)
                leftResult <- recursiveInfix(
                  newLeft.expr,
                  newLeft.rmn,
                  minPrecenden
                )
              } yield (leftResult)

            } else {
              constructOutPut(left, tokens).pure[F]
            }
        } yield (parserOut)
      }
      case Nil => constructOutPut(left, tokens).pure[F]
    }
  }

  override def parsePrecedence[F[_]](
      precedence: Precendence,
      tokens: List[Token]
  )(implicit
      a: MonadError[F, CompilerError]
  ): F[ParserResult[Typ]] = {
    for {
      prefixParser <- baseGrammar.mPrefixParsers(tokens)
      leftExpr <- prefixParser.parse(tokens)
      parserOut <- parseInfix(precedence, leftExpr.expr, leftExpr.rmn)
    } yield parserOut

  }

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
