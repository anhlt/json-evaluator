package io.jseval.parser

import cats.MonadError
import io.jseval.Parser.ParserOut
import io.jseval.{Token, CompilerError}
import cats.implicits._
import io.jseval.parser.AndInfixParser.precedence
import io.jseval.Expression.{Expr, App}
import cats.parse.Parser
import io.jseval.parser.Utils._
import io.jseval.Operator

case class JSParser() {

  def expression[F[_]](
      tokens: List[Token],
      precedence: Precendence = Precendence.LOWEST
  )(implicit
      a: MonadError[F, CompilerError]
  ): F[ParserOut] =
    app(tokens).orElse(parsePrecedence(precedence, tokens))

  implicit val jsParser: JSParser = this

  def app[F[_]](
      tokens: List[Token]
  )(implicit
      me: MonadError[F, CompilerError],
      jsParser: JSParser
  ): F[ParserOut] = {

    for {
      bodyAndRmn <- IdentifierParser.parse(tokens)
      leftParenAndRmn <- consume(Operator.LeftParen, bodyAndRmn.rmn)
      (_, rmnAfterLP) = leftParenAndRmn
      argAndRemaining <- appArgs(bodyAndRmn.expr, rmnAfterLP)
      rParenAndRmn <- consume(Operator.RightParen, argAndRemaining.rmn)
    } yield ParserOut(argAndRemaining.expr, rParenAndRmn._2)

  }

  def appArgs[F[_]](
      previousExpr: Expr,
      tokens: List[Token]
  )(implicit me: MonadError[F, CompilerError]): F[ParserOut] = {

    for {
      argAndRemaining <- expression(tokens)
      nextResult <- (for {
        commaAndTokens <- consume(Operator.Comma, argAndRemaining.rmn)
        (comma, afterComma) = commaAndTokens
        rs <- appArgs(App(previousExpr, argAndRemaining.expr), afterComma)
      } yield (rs)).recover({ case CompilerError.ExpectToken(Operator.Comma) =>
        ParserOut(App(previousExpr, argAndRemaining.expr), argAndRemaining.rmn)
      })

    } yield nextResult

  }

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
                infixParser <- Grammar.mInfixParser(tokens)
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
