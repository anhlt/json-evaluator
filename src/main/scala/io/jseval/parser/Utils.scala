package io.jseval.parser

import io.jseval.CompilerError
import cats._
import io.jseval.Expression.Variable
import io.jseval.Token
import io.jseval.Expression.Expr

case object Utils {
  def asVariable[F[_]](x: Expr)(implicit
      a: MonadError[F, CompilerError]
  ): F[Variable] = {
    x.match {
      case v: Variable => a.pure(v)
      case _           => a.raiseError(CompilerError.ExpectIdentifer())
    }
  }

  def consume[F[_]](
      expect: Token,
      tokens: List[Token]
  )(implicit a: MonadError[F, CompilerError]): F[(Token, List[Token])] =
    tokens match {
      case token :: rest if token == expect => a.pure((expect, rest))
      case _ => a.raiseError(CompilerError.ExpectToken(expect, tokens))
    }
}
