package io.jseval
import cats.syntax.all._

import io.jseval.Expression.BuildinFn.Arthimetric
import cats.syntax.flatMap

type LiteralType = Double | Boolean | String | Null

object Expression {

  sealed trait BuildinFn

  object BuildinFn {

    object ArthimetricFn {
      def apply(fn: ArthimetricFn)(a: Double)(b: Double): Double = {
        fn match {
          case Add => a + b
          case Sub => a - b
          case Mul => a * b
          case Div => a / b
        }
      }
    }

    sealed trait ArthimetricFn

    case object Add extends ArthimetricFn
    case object Sub extends ArthimetricFn
    case object Mul extends ArthimetricFn
    case object Div extends ArthimetricFn

    sealed trait ComparisonFn

    object ComparisonFn {
      def apply(fn: ComparisonFn)(a: Double)(b: Double): Boolean = {
        fn match {
          case Less    => a < b
          case Equal   => a == b
          case Greater => a > b
        }
      }
    }

    case object Less extends ComparisonFn
    case object Equal extends ComparisonFn
    case object Greater extends ComparisonFn

    case class Arthimetric(fn: ArthimetricFn, opA: Expr, opB: Expr)
        extends BuildinFn

    case class Comparison(fn: ComparisonFn, opA: Expr, opB: Expr)
        extends BuildinFn
  }

  sealed trait Error

  case class WrongType(v: Any, expectedType: String) extends Error

  sealed trait Expr

  object Expr {
    def asDouble(value: Any): Either[Error, Double] = {
      value match {
        case LiteralExpr(v: Double) => v.asRight
        case _                      => WrongType(value, "Double").asLeft

      }
    }

    def asBool(value: Any): Either[Error, Boolean] = {
      value match {
        case LiteralExpr(v: Boolean) => v.asRight
        case _                       => WrongType(value, "Boolean").asLeft

      }
    }

  }

  case class Variable(name: Token) extends Expr
  case class Assign(variable: Variable, body: Expr) extends Expr
  case class LiteralExpr(value: LiteralType) extends Expr
  case class Buildin(fn: BuildinFn) extends Expr
  case class Cond(pred: Expr, trueBranch: Expr, falseBranch: Expr) extends Expr

  // case class Add(left: Expr, right: Expr) extends Expr

  // case class Subtract(left: Expr, right: Expr) extends Expr

  // case class Multiply(left: Expr, right: Expr) extends Expr

  // case class Divide(left: Expr, right: Expr) extends Expr

  // case class Greater(left: Expr, right: Expr) extends Expr

  // case class GreaterEqual(left: Expr, right: Expr) extends Expr

  // case class Less(left: Expr, right: Expr) extends Expr

  // case class LessEqual(left: Expr, right: Expr) extends Expr

  // case class Equal(left: Expr, right: Expr) extends Expr

  // case class NotEqual(left: Expr, right: Expr) extends Expr

  // // Logic
  // case class And(left: Expr, right: Expr) extends Expr

  // case class Or(left: Expr, right: Expr) extends Expr

  // case class Negate(expr: Expr) extends Expr

  // case class Not(expr: Expr) extends Expr

  // case class Grouping(expr: Expr) extends Expr

}

object Evaluator {

  import Expression._
  import BuildinFn._

  def eval(expr: Expr): Either[Error, Expr] =
    expr match {
      case LiteralExpr(_) => expr.asRight
      case Buildin(Arthimetric(fn, opA, opB)) => {
        for {
          valA <- eval(opA) flatMap Expr.asDouble
          valB <- eval(opB) flatMap Expr.asDouble
        } yield LiteralExpr(ArthimetricFn.apply(fn)(valA)(valB))
      }

      case Buildin(Comparison(fn, opA, opB)) => {
        for {
          valA <- eval(opA) flatMap Expr.asDouble
          valB <- eval(opB) flatMap Expr.asDouble
        } yield LiteralExpr(ComparisonFn.apply(fn)(valA)(valB))
      }

      case Cond(pred, trueBranch, falseBranch) => {
        for {
          valPred <- eval(pred) flatMap Expr.asBool
          result <- eval(trueBranch)
        } yield trueBranch
      }

      // case _ => LiteralExpr(1.0).asRight
    }
}
