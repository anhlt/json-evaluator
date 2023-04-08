package io.jseval.parser

import cats._
import cats.implicits._
import io.jseval.CompilerError.NoExpectedParser
import io.jseval.{CompilerError, Keyword, Literal, Operator, Token}

object Grammar {
  def mPrefixParsers[F[_]](
      tokens: List[Token]
  )(implicit a: MonadError[F, CompilerError]): F[PrefixParser] = {

    tokens match
      case Literal.Number(_) :: rest    => a.pure(LiteralParser())
      case Literal.Str(_) :: rest       => a.pure(LiteralParser())
      case Keyword.True :: rest         => a.pure(LiteralParser())
      case Keyword.False :: rest        => a.pure(LiteralParser())
      case Operator.LeftParen :: rest   => a.pure(ParenthesisParser())
      case Operator.LeftBracket :: rest => a.pure(BracketPrefixParser())
      case Operator.LeftBrace :: rest   => a.pure(BracePrefixParser())

      case _ => a.raiseError(NoExpectedParser(tokens))
  }

  def mInfixParser[F[_]](
      tokens: List[Token]
  )(implicit a: MonadError[F, CompilerError]): F[InfixParser] = {
    tokens match
      case Keyword.Or :: rest     => OrInfixParser.pure[F]
      case Keyword.And :: rest    => AndInfixParser.pure[F]
      case Operator.Plus :: rest  => PlusInfixParser.pure[F]
      case Operator.Minus :: rest => MinusInfixParser.pure[F]
      case Operator.Star :: rest  => MultiplyInfixParser.pure[F]
      case Operator.Slash :: rest => DivideInfixParser.pure[F]
      case _                      => a.raiseError(NoExpectedParser(tokens))
  }

  def getPrecedence[F[_]](
      tokens: List[Token]
  )(implicit a: MonadError[F, CompilerError]): F[Precendence] = {
    for {
      infix <- mInfixParser(tokens)
    } yield infix.precedence
  }

}
