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
      case Keyword.TrueKw :: rest      => LiteralParser.pure[F]
      case Keyword.FalseKw :: rest     => LiteralParser.pure[F]

      case Literal.Identifier(name) :: rest => IdentifierParser.pure[F]
      case Operator.LeftParenToken :: rest       => ParenthesisParser.pure[F]
      case Operator.LeftBracketToken :: rest     => BracketPrefixParser.pure[F]
      case Operator.LeftBraceToken :: rest       => BracePrefixParser.pure[F]
      case Operator.BangToken :: rest            => UnaryPrefixParser.pure[F]
      case Operator.MinusToken :: rest           => UnaryPrefixParser.pure[F]
      case Keyword.IfKw :: rest               => ConditionPrefixParser.pure[F]
      case Keyword.FunKw :: rest              => FunctionPrefixParser.pure[F]
      case Keyword.LetKw :: rest              => LetBindingPrefixParser.pure[F]

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
      case Keyword.OrKw :: rest          => OrInfixParser.pure[F]
      case Keyword.AndKw :: rest         => AndInfixParser.pure[F]
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
