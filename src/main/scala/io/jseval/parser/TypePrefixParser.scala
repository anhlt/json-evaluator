package io.jseval.parser

import io.jseval.TypeError
import io.jseval.TypModule.Typ
import io.jseval.Token
import cats.MonadError
import io.jseval.TypModule._
import io.jseval.Keyword
import cats._
import cats.implicits._
import io.jseval.CompilerError

trait TypePrefixParser extends PrefixParser[Typ]

case object PrimaryParser extends TypePrefixParser {
  def parse[F[_]](tokens: List[Token])(implicit
      me: MonadError[F, CompilerError]
  ): F[ParserResult[Typ]] = {
    tokens match {
      case Keyword.Unit :: rest      => TypeParserResult(TUnit, rest).pure[F]
      case Keyword.IntKw :: rest     => TypeParserResult(TInt, rest).pure[F]
      case Keyword.BooleanKw :: rest => TypeParserResult(TBoolean, rest).pure[F]
      case Keyword.StringKw :: rest  => TypeParserResult(TString, rest).pure[F]
    }
  }
}
