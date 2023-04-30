package io.jseval.parser

import cats.MonadError
import io.jseval.{CompilerError, Token}
import io.jseval.Expression.BuildinModule.BuildinFn
import io.jseval.Expression.{Buildin, Expr}
import io.jseval.parser.{ParserOut, ParserResult}
import cats.implicits._
import io.jseval.Expression.TupleExpr

trait InfixParser[T] {
  val parser: (T, T) => T
  val precedence: Precendence

  def parse[F[_]](ts: List[Token], leftExpr: T)(implicit
      a: MonadError[F, CompilerError]
  ): F[ParserResult[T]]
}

trait InfixExprParser extends InfixParser[Expr] {
  val parser: (Expr, Expr) => Expr
  val precedence: Precendence

  def parse[F[_]](ts: List[Token], leftExpr: Expr)(implicit
      a: MonadError[F, CompilerError]
  ): F[ParserResult[Expr]] = {
    for {
      rightExprAndRemaining <- ExpressionParser.expression(ts, precedence = precedence)
    } yield (ParserOut(
      parser(leftExpr, rightExprAndRemaining.expr),
      rightExprAndRemaining.rmn
    ))
  }
}

case object OrInfixParser extends InfixExprParser:
  val parser: (Expr, Expr) => Expr =
    (l, r) => Buildin(BuildinFn.Logical(BuildinFn.Or, l, r))

  val precedence: Precendence = Precendence.LOGICAL_OR

case object AndInfixParser extends InfixExprParser:
  val parser: (Expr, Expr) => Expr =
    (l, r) => Buildin(BuildinFn.Logical(BuildinFn.And, l, r))
  val precedence: Precendence = Precendence.LOGICAL_AND

case object PlusInfixParser extends InfixExprParser:
  val parser: (Expr, Expr) => Expr =
    (l, r) => Buildin(BuildinFn.Arithmetic(BuildinFn.Add, l, r))
  val precedence: Precendence = Precendence.TERM

case object MinusInfixParser extends InfixExprParser:
  val parser: (Expr, Expr) => Expr =
    (l, r) => Buildin(BuildinFn.Arithmetic(BuildinFn.Sub, l, r))
  val precedence: Precendence = Precendence.TERM

case object MultiplyInfixParser extends InfixExprParser:
  val parser: (Expr, Expr) => Expr =
    (l, r) => Buildin(BuildinFn.Arithmetic(BuildinFn.Mul, l, r))
  val precedence: Precendence = Precendence.PRODUCT

case object DivideInfixParser extends InfixExprParser:
  val parser: (Expr, Expr) => Expr =
    (l, r) => Buildin(BuildinFn.Arithmetic(BuildinFn.Div, l, r))
  val precedence: Precendence = Precendence.PRODUCT

case object GreaterThanInfixParser extends InfixExprParser:
  val parser: (Expr, Expr) => Expr =
    (l, r) => Buildin(BuildinFn.Comparison(BuildinFn.Greater, l, r))
  val precedence: Precendence = Precendence.COMPARISON

case object GreaterThanOrEqualInfixParser extends InfixExprParser:
  val parser: (Expr, Expr) => Expr =
    (l, r) => Buildin(BuildinFn.Comparison(BuildinFn.GreaterEqual, l, r))
  val precedence: Precendence = Precendence.COMPARISON

case object LessThanInfixParser extends InfixExprParser:
  val parser: (Expr, Expr) => Expr =
    (l, r) => Buildin(BuildinFn.Comparison(BuildinFn.Less, l, r))
  val precedence: Precendence = Precendence.COMPARISON

case object LessThanOrEqualInfixParser extends InfixExprParser:
  val parser: (Expr, Expr) => Expr =
    (l, r) => Buildin(BuildinFn.Comparison(BuildinFn.LessEqual, l, r))
  val precedence: Precendence = Precendence.COMPARISON

case object EqualInfixParser extends InfixExprParser:
  val parser: (Expr, Expr) => Expr =
    (l, r) => Buildin(BuildinFn.Comparison(BuildinFn.Equal, l, r))
  val precedence: Precendence = Precendence.EQUALITY

case object NotEqualInfixParser extends InfixExprParser:
  val parser: (Expr, Expr) => Expr =
    (l, r) => Buildin(BuildinFn.Comparison(BuildinFn.NotEqual, l, r))
  val precedence: Precendence = Precendence.EQUALITY

case object TupleInfixParser extends InfixExprParser:
  val parser: (Expr, Expr) => Expr =
    (l, r) => TupleExpr(l, r)
  val precedence: Precendence = Precendence.RECORD
