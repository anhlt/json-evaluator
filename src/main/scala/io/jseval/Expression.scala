package io.jseval

import cats._
import cats.implicits._
import TypModule._


type LiteralType = Double | Boolean | String
object LiteralType {
  implicit def asType(v: LiteralType): Typ = {
    v match {
      case x: Double  => TInt
      case _: String  => TString
      case _: Boolean => TBoolean
    }

  }
}

object Expression {

  object BuildinModule {
    sealed trait BuildinFn

    object BuildinFn {

      sealed trait LogicalFn

      case object And extends LogicalFn
      case object Or extends LogicalFn

      object LogicalFn {
        def apply(
            fn: LogicalFn
        )(a: LiteralType)(b: LiteralType): LiteralType = {
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

      case class Arithmetic(fn: ArthimetricFn, opA: Expr, opB: Expr)
          extends BuildinFn

      case class Comparison(fn: ComparisonFn, opA: Expr, opB: Expr)
          extends BuildinFn

      case class Logical(fn: LogicalFn, opA: Expr, opB: Expr) extends BuildinFn

    }
  }

  object ValueModule {

    sealed trait Value
    object Value {

      def asDouble[F[_]](
          value: Any
      )(implicit a: MonadError[F, CompilerError]): F[Double] = {
        value match {
          case LiteralValue(v: Double) => a.pure(v)
          case _ => a.raiseError(CompilerError.WrongType(value, "Double"))
          // case _ => fr.raiseError(WrongType(value, "Double"))

        }
      }

      def asBool[F[_]](
          value: Any
      )(implicit a: MonadError[F, CompilerError]): F[Boolean] = {
        value match {
          case LiteralValue(v: Boolean) => a.pure(v)
          case _ => a.raiseError(CompilerError.WrongType(value, "Boolean"))
        }
      }

      def asClosure[F[_]](
          value: Any
      )(implicit a: MonadError[F, CompilerError]): F[Closure] = {
        value match {
          case v: Closure => a.pure(v)
          case _          => a.raiseError(CompilerError.WrongType(value, "Closure"))
        }
      }
    }

    case class LiteralValue(v: LiteralType) extends Value

    case class Closure(env: Env, varName: Token, body: Expr) extends Value

  }

  // var x , y
  // abs: \x = x + 1
  // app: \x = x + 1 , 5

  import ValueModule._
  import BuildinModule._

  type Env = Map[Token, Value]

  sealed trait Expr

  case class Variable(name: Token) extends Expr
  case class Abs(variableName: Variable, variableType: Typ, body: Expr)
      extends Expr
  case class App(body: Expr, arg: Expr) extends Expr
  case class LiteralExpr(value: LiteralType) extends Expr
  case class Buildin(fn: BuildinFn) extends Expr
  case class Cond(pred: Expr, trueBranch: Expr, falseBranch: Expr) extends Expr

  case class Grouping(expr: Expr) extends Expr
  case class Binding(
      recursive: Boolean,
      variableName: Variable,
      body: Expr,
      expr: Expr
  ) extends Expr

  // let f x = x + 5
  // recursive = 0 , var = f , body = \x = x + 5, expr = app f x
  //

}
