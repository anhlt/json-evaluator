package io.jseval.parser

import cats._
import cats.implicits._
import io.jseval.CompilerError.{NoExpectedParser, NoExpectedInfixParser}
import io.jseval.{CompilerError, Keyword, Literal, Operator, Token}



object Grammar {
  def mPrefixParsers[F[_]](
      tokens: List[Token]
  )(implicit a: MonadError[F, CompilerError]): F[PrefixExprParser] = {

    tokens match
      case Literal.Number(_) :: rest => LiteralParser.pure[F]
      case Literal.Str(_) :: rest    => LiteralParser.pure[F]
      case Keyword.True :: rest      => LiteralParser.pure[F]
      case Keyword.False :: rest     => LiteralParser.pure[F]

      case Literal.Identifier(name) :: rest => IdentifierParser.pure[F]
      case Operator.LeftParen :: rest       => ParenthesisParser.pure[F]
      case Operator.LeftBracket :: rest     => BracketPrefixParser.pure[F]
      case Operator.LeftBrace :: rest       => BracePrefixParser.pure[F]
      case Operator.Bang :: rest            => UnaryPrefixParser.pure[F]
      case Operator.Minus :: rest           => UnaryPrefixParser.pure[F]
      case Keyword.If :: rest               => ConditionPrefixParser.pure[F]
      case Keyword.Fun :: rest              => FunctionPrefixParser.pure[F]
      case Keyword.Let :: rest              => LetBindingPrefixParser.pure[F]

      case _ => a.raiseError(NoExpectedParser(tokens))
  }

  // def typePrefixParser[F[_]](
  //     tokens: List[Token]
  // )(implicit a: MonadError[F, CompilerError]): F[TypePrefixParser] = {

  //   tokens match {
  //     case Keyword.Unit :: rest    => DumpTypeParser.pure[F]
  //     case Keyword.Int :: rest     => DumpTypeParser.pure[F]
  //     case Keyword.Boolean :: rest => DumpTypeParser.pure[F]
  //     case Keyword.String :: rest  => DumpTypeParser.pure[F]
  //     case _                       => a.raiseError(NoExpectedParser(tokens))
  //   }
  // }

  // def typeInfixParser[F[_]](
  //     tokens: List[Token]
  // )(implicit a: MonadError[F, CompilerError]): F[TypePrefixParser] = {

  //   tokens match {
  //     case Keyword.Unit :: rest    => DumpTypeParser.pure[F]
  //     case Keyword.Int :: rest     => DumpTypeParser.pure[F]
  //     case Keyword.Boolean :: rest => DumpTypeParser.pure[F]
  //     case Keyword.String :: rest  => DumpTypeParser.pure[F]
  //     case _                       => a.raiseError(NoExpectedParser(tokens))
  //   }
  // }

  def mInfixParser[F[_]](
      tokens: List[Token]
  )(implicit a: MonadError[F, CompilerError]): F[InfixExprParser] = {
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

      case Operator.Comma :: rest => TupleInfixParser.pure[F]
      case _                      => a.raiseError(NoExpectedInfixParser(tokens))
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
