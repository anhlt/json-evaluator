package io.jseval.parser

import cats._
import cats.implicits._
import io.jseval.CompilerError.{NoExpectedParser, NoExpectedInfixParser}
import io.jseval.{CompilerError, Keyword, Literal, Operator, Token}
import io.jseval.Expression.Expr

trait BaseGrammar[T] {

  def mPrefixParsers[F[_]](
      tokens: List[Token]
  )(implicit a: MonadError[F, CompilerError]): F[PrefixParser[T]]

  def mInfixParsers[F[_]](
      tokens: List[Token]
  )(implicit a: MonadError[F, CompilerError]): F[InfixParser[T]]

  def getPrecedence[F[_]](
      tokens: List[Token]
  )(implicit a: MonadError[F, CompilerError]): F[Precendence]

}

object Grammar extends BaseGrammar[Expr] {
  def mPrefixParsers[F[_]](
      tokens: List[Token]
  )(implicit a: MonadError[F, CompilerError]): F[PrefixParser[Expr]] = {

    tokens match
      case Literal.Number(_) :: rest      => LiteralParser.pure[F]
      case Literal.FloatNumber(_) :: rest => LiteralParser.pure[F]
      case Literal.Str(_) :: rest         => LiteralParser.pure[F]
      case Keyword.TrueKw :: rest         => LiteralParser.pure[F]
      case Keyword.FalseKw :: rest        => LiteralParser.pure[F]

      case Literal.Identifier(name) :: rest  => IdentifierParser.pure[F]
      case Operator.LeftParenToken :: rest   => ParenthesisParser.pure[F]
      case Operator.LeftBracketToken :: rest => BracketPrefixParser.pure[F]
      case Operator.LeftBraceToken :: rest   => BracePrefixParser.pure[F]
      case Operator.BangToken :: rest        => UnaryPrefixParser.pure[F]
      case Operator.MinusToken :: rest       => UnaryPrefixParser.pure[F]
      case Keyword.IfKw :: rest              => ConditionPrefixParser.pure[F]
      case Keyword.FunKw :: rest             => FunctionPrefixParser.pure[F]
      case Keyword.LetKw :: rest             => LetBindingPrefixParser.pure[F]

      case _ => a.raiseError(NoExpectedParser(tokens))
  }

  def mInfixParsers[F[_]](
      tokens: List[Token]
  )(implicit a: MonadError[F, CompilerError]): F[InfixParser[Expr]] = {
    tokens match
      case Keyword.OrKw :: rest             => OrInfixParser.pure[F]
      case Keyword.AndKw :: rest            => AndInfixParser.pure[F]
      case Operator.PlusToken :: rest       => PlusInfixParser.pure[F]
      case Operator.MinusToken :: rest      => MinusInfixParser.pure[F]
      case Operator.StarToken :: rest       => MultiplyInfixParser.pure[F]
      case Operator.SlashToken :: rest      => DivideInfixParser.pure[F]
      case Operator.EqualEqualToken :: rest => EqualInfixParser.pure[F]
      case Operator.BangEqualToken :: rest  => NotEqualInfixParser.pure[F]
      case Operator.LessToken :: rest       => LessThanInfixParser.pure[F]
      case Operator.GreaterToken :: rest    => GreaterThanInfixParser.pure[F]
      case Operator.GreaterEqualToken :: rest =>
        GreaterThanOrEqualInfixParser.pure[F]
      case Operator.LessEqualToken :: rest => LessThanOrEqualInfixParser.pure[F]

      case Operator.CommaToken :: rest => TupleInfixParser.pure[F]
      case _ => a.raiseError(NoExpectedInfixParser(tokens))
  }

  def getPrecedence[F[_]](
      tokens: List[Token]
  )(implicit a: MonadError[F, CompilerError]): F[Precendence] = {
    (for {
      infix <- mInfixParsers(tokens)
    } yield infix.precedence).recoverWith({ case _ =>
      a.pure(Precendence.LOWEST)
    })
  }

}
