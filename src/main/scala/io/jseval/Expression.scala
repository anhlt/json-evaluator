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
          case null       => false
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

    object UnaryFn {
      def apply(fn: UnaryFn)(a: Double): Double = {
        fn match {
          case Not    => -a
          case Negate => -a
        }
      }
    }

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
  object Value {

    def asDouble[F[_]](
        value: Any
    )(implicit me: MonadError[F, Error]): F[Double] = {
      value match {
        case LiteralValue(v: Double) => me.pure(v)
        case _ => me.raiseError(WrongType(value, "Double"))

      }
    }

    def asBool[F[_]](
        value: Any
    )(implicit me: MonadError[F, Error]): F[Boolean] = {
      value match {
        case LiteralValue(v: Boolean) => me.pure(v)
        case _ => me.raiseError(WrongType(value, "Boolean"))
      }
    }

    def asClosure[F[_]](
        value: Any
    )(implicit me: MonadError[F, Error]): F[Closure] = {
      value match {
        case v: Closure => me.pure(v)
        case _          => me.raiseError(WrongType(value, "Closure"))
      }
    }
  }

  case class LiteralValue(v: LiteralType) extends Value

  case class Closure(env: Env, varName: Token, body: Expr) extends Value

  type Env = Map[Token, Value]

  sealed trait Error

  case class WrongType(v: Any, expectedType: String) extends Error
  case class UnboundedName(token: Token) extends Error

  // var x , y
  // abs: \x = x + 1
  // app: \x = x + 1 , 5

  sealed trait Expr

  case class Variable(name: Token) extends Expr
  case class Abs(variableName: Token, body: Expr) extends Expr
  case class App(body: Expr, arg: Expr) extends Expr
  case class LiteralExpr(value: LiteralType) extends Expr
  case class Buildin(fn: BuildinFn) extends Expr
  case class Cond(pred: Expr, trueBranch: Expr, falseBranch: Expr) extends Expr

  case class Grouping(expr: Expr) extends Expr
  case class Binding(
      recursive: Boolean,
      variableName: Token,
      body: Expr,
      expr: Expr
  ) extends Expr

  // let f x = x + 5
  // recursive = 0 , var = f , body = \x = x + 5, expr = app f x
  //

}
