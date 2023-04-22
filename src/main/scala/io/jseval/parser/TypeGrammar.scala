package io.jseval.parser

import io.jseval.TypModule.Typ
import io.jseval.{CompilerError, Keyword, Operator}
import cats._
import cats.implicits._
import io.jseval.Token

object TypeGrammar extends BaseGrammar[Typ] {

  override def mPrefixParsers[F[_]](tokens: List[Token])(implicit
      a: MonadError[F, CompilerError]
  ): F[PrefixParser[Typ]] = {
    tokens match {
      case Keyword.IntKw :: rest =>
        PrimaryParser.pure[F]
      case Keyword.BooleanKw :: rest =>
        PrimaryParser.pure[F]
      case Keyword.StringKw :: rest =>
        PrimaryParser.pure[F]
      case Keyword.Unit :: rest =>
        PrimaryParser.pure[F]

      case _ => a.raiseError(CompilerError.NoExpectedParser(tokens))
    }
  }

  override def mInfixParsers[F[_]](tokens: List[Token])(implicit
      a: MonadError[F, CompilerError]
  ): F[InfixParser[Typ]] =
    tokens match {

      case Operator.ArrowToken :: rest =>
        ArrowInfixParser.pure[F]
      case Operator.StarToken :: rest =>
        ProductInfixParser.pure[F]

      case _ => a.raiseError(CompilerError.NoExpectedInfixParser(tokens))
    }

  override def getPrecedence[F[_]](tokens: List[Token])(implicit
      a: MonadError[F, CompilerError]
  ): F[Precendence] =
    (for {
      infix <- mInfixParsers(tokens)
    } yield infix.precedence).recoverWith({ case _ =>
      a.pure(Precendence.LOWEST)
    })

}
