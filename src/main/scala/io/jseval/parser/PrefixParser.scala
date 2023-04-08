package io.jseval.parser

import cats.MonadError
import io.jseval.Parser.ParserOut
import io.jseval.Token
import io.jseval.CompilerError
import io.jseval.Literal
import io.jseval.Expression
import io.jseval.Keyword

trait PrefixParser {
  def parse[F[_]](tokens: List[Token])(implicit a: MonadError[F, CompilerError], JSParser: JSParser): F[ParserOut]
}


case class LiteralParser() extends PrefixParser {

  def parse[F[_]](tokens: List[Token])(implicit a: MonadError[F, CompilerError], JSParser: JSParser): F[ParserOut] = {
    tokens match
      case Literal.Number(l) :: rest =>
        a.pure(ParserOut(Expression.LiteralExpr(l.toDouble), rest))
      case Literal.Str(l) :: rest    =>
        a.pure(ParserOut(Expression.LiteralExpr(l), rest))
      case Keyword.True :: rest      =>
        a.pure(ParserOut(Expression.LiteralExpr(true), rest))
      case Keyword.False :: rest     =>
        a.pure(ParserOut(Expression.LiteralExpr(false), rest))
      case _                         => a.raiseError(CompilerError.ExpectExpression(tokens))
  }

}

case class ParenthesisParser() extends PrefixParser {
  def parse[F[_]](tokens: List[Token])(implicit a: MonadError[F, CompilerError], JSParser: JSParser): F[ParserOut] = ???
}

case class BracketPrefixParser() extends PrefixParser {
  def parse[F[_]](tokens: List[Token])(implicit a: MonadError[F, CompilerError], JSParser: JSParser): F[ParserOut] = ???
}

case class BracePrefixParser() extends PrefixParser {
  def parse[F[_]](tokens: List[Token])(implicit a: MonadError[F, CompilerError], JSParser: JSParser): F[ParserOut] = ???
}