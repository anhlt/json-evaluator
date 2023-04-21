package io.jseval.parser

import io.jseval.TypeError
import io.jseval.TypModule.Typ
import io.jseval.Token
import cats.MonadError
import io.jseval.TypModule._
import io.jseval.Keyword
import cats._
import cats.implicits._

trait TypePrefixParser {
  def parseType[F[_]](tokens: List[Token])(implicit
      me: MonadError[F, TypeError]
  ): F[Typ]
}

case object DumpTypeParser extends TypePrefixParser {
  def parseType[F[_]](tokens: List[Token])(implicit
      me: MonadError[F, TypeError]
  ): F[Typ] = ???
}

case object PrimaryParser extends TypePrefixParser {
  def parseType[F[_]](tokens: List[Token])(implicit
      me: MonadError[F, TypeError]
  ): F[Typ] = {
    tokens match {
      case Keyword.Unit :: rest    => TUnit.pure[F]
      case Keyword.IntKw :: rest     => TInt.pure[F]
      case Keyword.BooleanKw :: rest => TBoolean.pure[F]
      case Keyword.StringKw :: rest  => TString.pure[F]
    }
  }
}

