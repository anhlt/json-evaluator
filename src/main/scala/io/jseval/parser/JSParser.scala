package io.jseval.parser

import cats.MonadError
import io.jseval.Parser.ParserOut
import io.jseval.{Token, CompilerError}
import cats.implicits._

case class JSParser() {

  def expression[F[_]](tokens: List[Token])(implicit a: MonadError[F, CompilerError]): F[ParserOut] = ???


  def parsePrecedence[F[_]](tokens: List[Token])(implicit a: MonadError[F, CompilerError]): F[ParserOut] = {

    for {
      prefixParser <- Grammar.mPrefixParsers(tokens)
      leftExpr <- prefixParser.parse(this, tokens)


    } yield leftExpr

  }

}
