package io.jseval.parser

import cats._
import cats.implicits._
import io.jseval.CompilerError.{NoExpectedParser, NoExpectedInfixParser}
import io.jseval.{CompilerError, Keyword, Literal, Operator, Token}

object Grammar {
  def mPrefixParsers[F[_]](
      tokens: List[Token]
  )(implicit a: MonadError[F, CompilerError]): F[PrefixParser] = {

    tokens match
      case Literal.Number(_) :: rest        => a.pure(LiteralParser())
      case Literal.Str(_) :: rest           => a.pure(LiteralParser())
      case Keyword.True :: rest             => a.pure(LiteralParser())
      case Keyword.False :: rest            => a.pure(LiteralParser())
      case Literal.Identifier(name) :: rest => IdentifierParser().pure[F]
      case Operator.LeftParen :: rest       => a.pure(ParenthesisParser())
      case Operator.LeftBracket :: rest     => a.pure(BracketPrefixParser())
      case Operator.LeftBrace :: rest       => a.pure(BracePrefixParser())
      case Operator.Bang :: rest            => a.pure(UnaryPrefixParser())
      case Operator.Minus :: rest           => UnaryPrefixParser().pure[F]

      case _ => a.raiseError(NoExpectedParser(tokens))
  }

  def mInfixParser[F[_]](
      tokens: List[Token]
  )(implicit a: MonadError[F, CompilerError]): F[InfixParser] = {
    tokens match
      case Keyword.Or :: rest          => OrInfixParser.pure[F]
      case Keyword.And :: rest         => AndInfixParser.pure[F]
      case Operator.Plus :: rest       => PlusInfixParser.pure[F]
      case Operator.Minus :: rest      => MinusInfixParser.pure[F]
      case Operator.Star :: rest       => MultiplyInfixParser.pure[F]
      case Operator.Slash :: rest      => DivideInfixParser.pure[F]
      case Operator.EqualEqual :: rest => EqualInfixParser.pure[F]
      case Operator.BangEqual :: rest  => NotEqualInfixParser.pure[F]
      case Operator.Less :: rest       => LessThanInfixParser.pure[F]
      case Operator.Greater :: rest    => GreaterThanInfixParser.pure[F]
      case Operator.GreaterEqual :: rest =>
        GreaterThanOrEqualInfixParser.pure[F]
      case Operator.LessEqual :: rest => LessThanOrEqualInfixParser.pure[F]

      case _ => a.raiseError(NoExpectedInfixParser(tokens))
  }

  def getPrecedence[F[_]](
      tokens: List[Token]
  )(implicit a: MonadError[F, CompilerError]): F[Precendence] = {
    (for {
      infix <- mInfixParser(tokens)
    } yield infix.precedence).recoverWith({ case _ =>
      a.pure(Precendence.LOWEST)
    })
  }

}
