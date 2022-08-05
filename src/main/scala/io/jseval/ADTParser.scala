package io.jseval

import io.jseval.Expression.Expr

trait ADTParser[T] {

  type ExprParser = Either[Error, (Expr, List[T])]

  enum Error(msg: String, tokens: List[T]):
    case ExpectExpression(tokens: List[T])
        extends Error("ExpectExpression", tokens)
    case ExpectClosing(tokens: List[T])
        extends Error("Expect ')' after expression", tokens)
    case InvalidAssignmentTartget(token: T)
        extends Error("Invalid assignment target.", List(token))

  def primary[F[_]](tokens: List[T])(implicit f: T => Token): ExprParser =
    tokens match
      case Literal.Number(l) :: rest =>
        Right(Expression.LiteralExpr(l.toDouble), rest)
      case Literal.Str(l) :: rest => Right(Expression.LiteralExpr(l), rest)
      case Keyword.True :: rest   => Right(Expression.LiteralExpr(true), rest)
      case Keyword.False :: rest  => Right(Expression.LiteralExpr(false), rest)
      case Keyword.Null :: rest   => Right(Expression.LiteralExpr(null), rest)
      case Literal.Identifier(name) :: rest =>
        Right(Expression.Variable(Literal.Identifier(name)), rest)
      case _ => Left(Error.ExpectExpression(tokens))

}
