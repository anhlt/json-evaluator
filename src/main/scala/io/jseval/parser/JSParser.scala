package io.jseval.parser

import cats.MonadError
import io.jseval.parser.ParserOut
import io.jseval.{Token, CompilerError}
import cats.implicits._
import io.jseval.parser.AndInfixParser.precedence
import io.jseval.Expression.{Expr, App}
import cats.parse.Parser
import io.jseval.parser.Utils._
import io.jseval.Operator
import io.jseval.TypModule._

case class JSParser() {

  def expression[F[_]](
      tokens: List[Token],
      precedence: Precendence = Precendence.LOWEST
  )(implicit
      a: MonadError[F, CompilerError]
  ): F[ParserOut] =
    parsePrecedence(precedence, tokens)

  implicit val jsParser: JSParser = this

  def parsePrecedence[F[_]](precedence: Precendence, tokens: List[Token])(
      implicit a: MonadError[F, CompilerError]
  ): F[ParserOut] = {

    for {
      prefixParser <- Grammar.mPrefixParsers(tokens)
      leftExpr <- prefixParser.parse(tokens)
      parserOut <- parseInfix(precedence, leftExpr.expr, leftExpr.rmn)
    } yield parserOut

  }

  def parseInfix[F[_]](
      minPrecenden: Precendence,
      left: Expr,
      tokens: List[Token]
  )(implicit a: MonadError[F, CompilerError]): F[ParserOut] = {
    recursiveInfix(left = left, tokens, minPrecenden = minPrecenden)
  }

  def recursiveInfix[F[_]](
      left: Expr,
      tokens: List[Token],
      minPrecenden: Precendence
  )(implicit a: MonadError[F, CompilerError]): F[ParserOut] = {
    // op : lookahead
    tokens match {
      case op :: rest => {
        for {
          opPrecedence <- Grammar.getPrecedence(tokens) // op precedence
          parserOut <-
            if (minPrecenden.code < opPrecedence.code) {
              for {
                infixParser: InfixExprParser <- Grammar.mInfixParser(tokens)
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
              ParserOut(left, tokens).pure[F]
            }
        } yield (parserOut)
      }
      case Nil => ParserOut(left, tokens).pure[F]
    }
  }
}
