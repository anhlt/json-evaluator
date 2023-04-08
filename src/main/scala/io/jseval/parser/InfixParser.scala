package io.jseval.parser

import cats.MonadError
import io.jseval.{CompilerError, Token}
import io.jseval.Expression.BuildinModule.BuildinFn
import io.jseval.Expression.{Buildin, Expr}
import io.jseval.Parser.ParserOut
import cats.implicits._

trait InfixParser {
  val parser: (Expr, Expr) => Expr
  val precedence: Precendence

  def parse[F[_]](ts: List[Token], leftExpr: Expr)(implicit a: MonadError[F, CompilerError], jpParser: JSParser): F[ParserOut] = {
    for {
      rightExprAndRemaining <- jpParser.parsePrecedence(precedence, ts)
    } yield (ParserOut(parser(leftExpr, rightExprAndRemaining.expr), rightExprAndRemaining.rmn))
  }
}

case object OrInfixParser extends InfixParser:
  val parser: (Expr, Expr) => Expr =
    (l, r) => Buildin(BuildinFn.Logical(BuildinFn.Or, l, r))

  val precedence: Precendence = Precendence.LOGICAL_OR

case object AndInfixParser extends InfixParser:
  val parser: (Expr, Expr) => Expr =
    (l, r) => Buildin(BuildinFn.Logical(BuildinFn.And, l, r))
  val precedence: Precendence = Precendence.LOGICAL_AND

case object PlusInfixParser extends InfixParser:
  val parser: (Expr, Expr) => Expr =
    (l, r) => Buildin(BuildinFn.Arithmetic(BuildinFn.Add, l, r))
  val precedence: Precendence = Precendence.TERM

case object MinusInfixParser extends InfixParser:
  val parser: (Expr, Expr) => Expr =
    (l, r) => Buildin(BuildinFn.Arithmetic(BuildinFn.Sub, l, r))
  val precedence: Precendence = Precendence.TERM

case object MultiplyInfixParser extends InfixParser:
  val parser: (Expr, Expr) => Expr =
    (l, r) => Buildin(BuildinFn.Arithmetic(BuildinFn.Mul, l, r))
  val precedence: Precendence = Precendence.PRODUCT

case object DivideInfixParser extends InfixParser:
  val parser: (Expr, Expr) => Expr =
    (l, r) => Buildin(BuildinFn.Arithmetic(BuildinFn.Div, l, r))
  val precedence: Precendence = Precendence.PRODUCT

case object GreaterThanInfixParser extends InfixParser:
  val parser: (Expr, Expr) => Expr =
    (l, r) => Buildin(BuildinFn.Comparison(BuildinFn.Greater, l, r))
  val precedence: Precendence = Precendence.COMPARISON

case object GreaterThanOrEqualInfixParser extends InfixParser:
  val parser: (Expr, Expr) => Expr =
    (l, r) => Buildin(BuildinFn.Comparison(BuildinFn.GreaterEqual, l, r))
  val precedence: Precendence = Precendence.COMPARISON

case object LessThanInfixParser extends InfixParser:
  val parser: (Expr, Expr) => Expr =
    (l, r) => Buildin(BuildinFn.Comparison(BuildinFn.Less, l, r))
  val precedence: Precendence = Precendence.COMPARISON

case object LessThanOrEqualInfixParser extends InfixParser:
  val parser: (Expr, Expr) => Expr =
    (l, r) => Buildin(BuildinFn.Comparison(BuildinFn.LessEqual, l, r))
  val precedence: Precendence = Precendence.COMPARISON

case object EqualInfixParser extends InfixParser:
  val parser: (Expr, Expr) => Expr =
    (l, r) => Buildin(BuildinFn.Comparison(BuildinFn.Equal, l, r))
  val precedence: Precendence = Precendence.EQUALITY

case object NotEqualInfixParser extends InfixParser:
  val parser: (Expr, Expr) => Expr =
    (l, r) => Buildin(BuildinFn.Comparison(BuildinFn.NotEqual, l, r))
  val precedence: Precendence = Precendence.EQUALITY

