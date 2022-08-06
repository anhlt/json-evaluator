package io.jseval
import cats.syntax.all._
import cats._
import cats.implicits._

type LiteralType = Double | Boolean | String | Null

object Expression {

  sealed trait BuildinFn

  object BuildinFn {

    sealed trait LogicalFn

    case object And extends LogicalFn
    case object Or extends LogicalFn

    object LogicalFn {
      def apply(fn: LogicalFn)(a: LiteralType)(b: LiteralType): LiteralType = {
        fn match {
          case And => if (isTruthy(a)) a else b
          case Or  => if (!isTruthy(a)) a else b
        }
      }

      def isTruthy(value: LiteralType): Boolean = {
        value match {
          case a: Double  => a != 0
          case a: Boolean => a
          case a: String  => a.length > 0
          case _          => false
        }
      }
    }

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
          case Less         => a < b
          case Equal        => a == b
          case Greater      => a > b
          case NotEqual     => a != b
          case LessEqual    => a <= b
          case GreaterEqual => a >= b
        }
      }
    }

    case object Less extends ComparisonFn
    case object Equal extends ComparisonFn
    case object Greater extends ComparisonFn
    case object NotEqual extends ComparisonFn
    case object LessEqual extends ComparisonFn
    case object GreaterEqual extends ComparisonFn

    sealed trait UnaryFn

    case object Not extends UnaryFn
    case object Negate extends UnaryFn

    case class Unary(fn: UnaryFn, opA: Expr) extends BuildinFn

    case class Arthimetric(fn: ArthimetricFn, opA: Expr, opB: Expr)
        extends BuildinFn

    case class Comparison(fn: ComparisonFn, opA: Expr, opB: Expr)
        extends BuildinFn

    case class Logical(fn: LogicalFn, opA: Expr, opB: Expr) extends BuildinFn

  }

  sealed trait Value

  case class LitValue(v: LiteralType) extends Value

  sealed trait Error

  case class WrongType(v: Any, expectedType: String) extends Error

  sealed trait Expr

  object Expr {
    def asDouble[F[_]](
        value: Any
    )(implicit me: MonadError[F, Error]): F[Double] = {
      value match {
        case LiteralExpr(v: Double) => me.pure(v)
        case _                      => me.raiseError(WrongType(value, "Double"))

      }
    }

    def asBool[F[_]](
        value: Any
    )(implicit me: MonadError[F, Error]): F[Boolean] = {
      value match {
        case LiteralExpr(v: Boolean) => me.pure(v)
        case _ => me.raiseError(WrongType(value, "Boolean"))
      }
    }

  }

  case class Variable(name: Token) extends Expr
  case class Assign(variable: Variable, body: Expr) extends Expr
  case class LiteralExpr(value: LiteralType) extends Expr
  case class Buildin(fn: BuildinFn) extends Expr
  case class Cond(pred: Expr, trueBranch: Expr, falseBranch: Expr) extends Expr

  case class Grouping(expr: Expr) extends Expr

}

object Evaluator {

  import Expression._
  import BuildinFn._

  def eval[F[_]](expr: Expr)(implicit me: MonadError[F, Error]): F[Expr] =
    expr match {
      case LiteralExpr(_) => me.pure(expr)
      case Buildin(Arthimetric(fn, opA, opB)) => {
        for {
          valA1 <- eval(opA)
          valA <- Expr.asDouble(valA1)
          valB1 <- eval(opB)
          valB <- Expr.asDouble(valB1)
        } yield LiteralExpr(ArthimetricFn.apply(fn)(valA)(valB))
      }

      case Buildin(Comparison(fn, opA, opB)) => {
        for {
          valA <- eval(opA) flatMap Expr.asDouble
          valB <- eval(opB) flatMap Expr.asDouble
        } yield LiteralExpr(ComparisonFn.apply(fn)(valA)(valB))
      }

      case Buildin(Logical(fn, opA, opB)) => {
        for {
          valA1 <- eval(opA)
          valA <- Expr.asDouble(valA1)
          valB1 <- eval(opB)
          valB <- Expr.asDouble(valB1)
        } yield LiteralExpr(LogicalFn.apply(fn)(valA)(valB))
      }

      case Cond(pred, trueBranch, falseBranch) => {
        for {
          valPredExpr <- eval(pred)
          valPred <- Expr.asBool(valPredExpr)
          result <- if (valPred) eval(trueBranch) else eval(falseBranch)
        } yield result
      }

      // case _ => LiteralExpr(1.0).asRight
    }
}
