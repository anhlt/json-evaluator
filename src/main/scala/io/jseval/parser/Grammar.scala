package io.jseval.parser

import cats.MonadError
import io.jseval.CompilerError.NoExpectedParser
import io.jseval.{CompilerError, Keyword, Literal, Operator, Token}

object Grammar {
  def mPrefixParsers[F[_]](tokens: List[Token])(implicit a: MonadError[F, CompilerError]): F[PrefixParser] = {

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

  def mInfixParser[F[_]](tokens: List[Token])(implicit a: MonadError[F, CompilerError]): F[InfixParser] = {
    tokens match
      case Keyword.Or :: rest  => a.pure(OrInfixParser)
      case Keyword.And :: rest => a.pure(AndInfixParser)
      case _ => a.raiseError(NoExpectedParser(tokens))
  }

}
