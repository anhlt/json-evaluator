package io.jseval.parser

import io.jseval.TypeError
import io.jseval.TypModule.Typ
import io.jseval.Token
import cats.MonadError
import io.jseval.TypModule._
import io.jseval.{Keyword, Operator, Literal}
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
      case _ => me.raiseError(CompilerError.ExpectExpression(tokens))
    }
  }
}

/*
  parser for generic type
  Like F# generic type means that we can have a type with a type parameter
  Example: 'a
  The ApostropeToken is a special character that is used to denote a generic type
  The parser will parse the type parameter and return the type
  Example: 'a -> TGeneric("a")
*/
case object GenericTypeParser extends TypePrefixParser {
  def parse[F[_]](tokens: List[Token])(implicit
      me: MonadError[F, CompilerError]
  ): F[ParserResult[Typ]] = {
    tokens match {
      case Operator.ApostropeToken :: Literal.Identifier(name) :: rest =>
        TypeParserResult(TGeneric(Literal.Identifier(name)), rest).pure[F]
      case _ => me.raiseError(CompilerError.ExpectExpression(tokens))
    }
  }
}


/*
  parser for parenthesis
  Example: (int -> int)
  The parser will parse the type inside the parenthesis and return the type
  Example: (int -> int) -> TInt -> TInt
*/
case object ParenthesisTypeParser extends TypePrefixParser {
  def parse[F[_]](tokens: List[Token])(implicit
      me: MonadError[F, CompilerError]
  ): F[ParserResult[Typ]] = {
    tokens match {
      case Operator.LeftParenToken :: rest =>
        for {
          typ <- TypeParser.parseType(rest)
          _ <- typ.rmn match {
            case Operator.RightParenToken :: rest => ().pure[F]
            case _ => me.raiseError(CompilerError.ExpectToken(Operator.RightParenToken,tokens))
          }
        } yield TypeParserResult(typ.expr, typ.rmn.tail)
      case _ => me.raiseError(CompilerError.ExpectToken(Operator.LeftParenToken, tokens))
    }
  }
}
