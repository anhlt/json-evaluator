package io.jseval.parser

import cats._
import cats.implicits._
import io.jseval.{CompilerError, Token}

trait BaseParser[T] {

  val baseGrammar: BaseGrammar[T]

  def constructOutPut(expr: T, rmn: List[Token]): ParserResult[T]

  def parseInfix[F[_]](
      minPrecenden: Precendence,
      left: T,
      tokens: List[Token]
  )(implicit a: MonadError[F, CompilerError]): F[ParserResult[T]] = {
    recursiveInfix(left = left, tokens, minPrecenden = minPrecenden)
  }

  def recursiveInfix[F[_]](
      left: T,
      tokens: List[Token],
      minPrecenden: Precendence
  )(implicit a: MonadError[F, CompilerError]): F[ParserResult[T]] = {
    // op : lookahead
    tokens match {
      case op :: rest => {
        for {
          opPrecedence <- baseGrammar.getPrecedence(tokens) // op precedence
          parserOut <-
            if (minPrecenden.code < opPrecedence.code) {
              for {
                infixParser <- baseGrammar.mInfixParsers(tokens)
                newLeft <- infixParser.parse(
                  rest,
                  left
                ) // parse right, and return result
                result <- recursiveInfix(
                  newLeft.expr,
                  newLeft.rmn,
                  minPrecenden
                )
              } yield result
            } else {
              constructOutPut(left, tokens).pure[F]
            }
        } yield (parserOut)
      }
      case Nil => constructOutPut(left, tokens).pure[F]
    }
  }

  def parsePrecedence[F[_]](precedence: Precendence, tokens: List[Token])(
      implicit a: MonadError[F, CompilerError]
  ): F[ParserResult[T]] = {

    for {
      prefixParser <- baseGrammar.mPrefixParsers(tokens)
      leftExpr <- prefixParser.parse(tokens)
      parserOut <- parseInfix(precedence, leftExpr.expr, leftExpr.rmn)
    } yield parserOut

  }
}
