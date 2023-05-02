package io.jseval.typeinfer

import io.jseval.Token
import io.jseval.TypModule.Typ
import io.jseval.Expression.Expr
import cats._
import cats.implicits._
import io.jseval.CompilerError
import io.jseval.Expression.TupleExpr
import io.jseval.TypModule.TProduct
import io.jseval.TypModule.TInt
import io.jseval.TypModule.TDouble
import io.jseval.TypModule.TString
import io.jseval.TypModule.TBoolean
import io.jseval.LiteralType
import io.jseval.TypModule.TArrow
import io.jseval.Expression.LiteralExpr
import io.jseval.Expression.Buildin
import io.jseval.Expression.BuildinModule.BuildinFn.ComparisonFn
import io.jseval.Expression.BuildinModule.BuildinFn.ArithmeticFn
import io.jseval.Expression.BuildinModule.BuildinFn.Comparison
import io.jseval.Expression.BuildinModule.BuildinFn.Arithmetic
import io.jseval.Expression.BuildinModule.BuildinFn.Unary
import io.jseval.Expression.BuildinModule.BuildinFn.UnaryFn
import io.jseval.Expression.BuildinModule.BuildinFn
import io.jseval.Expression.Cond
import io.jseval.Expression.Abs
import io.jseval.Expression.App
import io.jseval.Expression.Variable
import io.jseval.Expression.Binding

object TypeInfer {
  type TypeEnv = Map[Token, Typ]

  object Utils {

    def asType(v: LiteralType): Typ = {
      v match {
        case x: Int     => TInt
        case x: Double  => TDouble
        case _: String  => TString
        case _: Boolean => TBoolean
      }
    }

    def asArrow[F[_]](
        typ: Typ
    )(implicit me: MonadError[F, CompilerError]): F[TArrow] = {
      typ match {
        case x: TArrow => me.pure(x)
        case _ => me.raiseError(CompilerError.IncorrectType("TArrow", typ))
      }

    }

    def asBool[F[_]](
        typ: Typ
    )(implicit me: MonadError[F, CompilerError]): F[Typ] = {
      typ match {
        case TBoolean => me.pure(TBoolean)
        case _ => me.raiseError(CompilerError.IncorrectType("TBoolean", typ))
      }
    }

    def equals(typ1: Typ, typ2: Typ): Boolean = {
      (typ1, typ2) match {
        case (TInt, TInt)         => true
        case (TString, TString)   => true
        case (TBoolean, TBoolean) => true
        case (TDouble, TDouble)   => true
        case (t1: TArrow, t2: TArrow) =>
          equals(t1.argType, t2.argType) && equals(t1.bodyType, t2.bodyType)
        case (t1: TProduct, t2: TProduct) =>
          equals(t1.firstType, t2.firstType) && equals(
            t1.secondType,
            t2.secondType
          )
        case _ => false
      }
    }

    extension (fn: ArithmeticFn) {
      def infer[F[_]](
          a: Typ
      )(b: Typ)(implicit me: MonadError[F, CompilerError]): F[Typ] = {
        (a, b) match {
          case (TInt, TInt)       => TInt.pure[F]
          case (TDouble, TDouble) => TDouble.pure[F]
          case (TInt, TDouble)    => TDouble.pure[F]
          case (TDouble, TInt)    => TDouble.pure[F]
          case _ => me.raiseError(CompilerError.ArithmeticTypeException(a, b))
        }
      }
    }

    extension (fn: UnaryFn) {
      def infer[F[_]](
          a: Typ
      )(implicit me: MonadError[F, CompilerError]): F[Typ] = {
        fn match
          case BuildinFn.Negate => {
            a match
              case TInt    => TInt.pure[F]
              case TDouble => TDouble.pure[F]
              case _ =>
                me.raiseError(CompilerError.UnaryTypeException(fn, a))
          }
          case BuildinFn.Not => {
            a match
              case TBoolean => TBoolean.pure[F]
              case _ =>
                me.raiseError(CompilerError.UnaryTypeException(fn, a))
          }

      }
    }
  }

  import Utils._

  def infer[F[_]](
      expr: Expr
  )(implicit me: MonadError[F, CompilerError], env: TypeEnv): F[Typ] = {
    expr match
      case TupleExpr(leftExpr, rightExpr) =>
        for {
          leftType <- infer(leftExpr)
          rightType <- infer(rightExpr)
        } yield TProduct(leftType, rightType)

      case LiteralExpr(v) => me.pure(Utils.asType(v))

      case Buildin(Arithmetic(fn, opA, opB)) => {
        for {
          opAType <- infer(opA)
          opBType <- infer(opB)
          result <- fn.infer(opAType)(opBType)
        } yield result
      }

      case Buildin(Comparison(fn, opA, opB)) => {
        for {
          aAsValue <- infer(opA)
          bAsValue <- infer(opB)
          result <-
            if (Utils.equals(aAsValue, bAsValue)) {
              me.pure(TBoolean)
            } else {
              me.raiseError(
                CompilerError.ComparisonTypeException(aAsValue, bAsValue)
              )
            }
        } yield TBoolean
      }

      // unary
      case Buildin(Unary(fn, expr)) => {
        for {
          exprTyp <- infer(expr)
          result <- fn.infer(exprTyp)
        } yield result
      }

      // logical
      case Buildin(BuildinFn.Logical(fn, opA, opB)) => {
        for {
          aAsValue <- infer(opA)
          aAsDouble <- Utils.asBool(aAsValue)
          bAsValue <- infer(opB)
          bAsDouble <- Utils.asBool(bAsValue)
        } yield TBoolean
      }

      case Cond(pred, trueBranch, falseBranch) => {
        for {
          predType <- infer(pred)
          predTypeAsBool <- Utils.asBool(predType)
          trueBranchType <- infer(trueBranch)
          falseBranchType <- infer(falseBranch)
          result <-
            if (Utils.equals(trueBranchType, falseBranchType)) {
              me.pure(trueBranchType)
            } else {
              me.raiseError(
                CompilerError.IncorrectType(s"$trueBranchType", falseBranchType)
              )
            }
        } yield result
      }

      // The type of a function will be TArrow
      // fun x : int = x + 5
      // we need to add the type of the variable to the environment
      // so that we can use it in the body
      // then we need to infer the type of the body
      // and return the type of the function
      case Abs(variable, variableType, body) => {

        variableType match {
          case Some(valType) =>
            val enclosedEnv = env + (variable.name -> valType)
            infer(body)(me, enclosedEnv).flatMap(bodyType =>
              me.pure(
                TArrow(valType, bodyType)
              )
            )
          case None =>
            me.raiseError(CompilerError.MissingTypeAnnotation(variable.name))
        }
      }

      case variable @ Variable(token) =>
        env.get(token) match {
          case Some(v) => me.pure(v)
          case None =>
            me.raiseError(CompilerError.UnboundedNameWhileTypeCheck(token))
        }

      // Type infer for function application
      // we need to infer the type of the function
      // and the type of the argument
      // and then return the type of the body
      // example let f = fun x : int = x + 5 in f(3)
      // we need to infer the type of f and 3
      // and then return the type of the body
      case App(fn, arg) => {
        for {
          fnType <- infer(fn)
          argType <- infer(arg)
          fnTypeAsArrow <- Utils.asArrow(fnType)
          result <-
            if (Utils.equals(argType, fnTypeAsArrow.argType)) {
              me.pure(fnTypeAsArrow.bodyType)
            } else {
              me.raiseError(
                CompilerError.IncorrectType(
                  s"${fnTypeAsArrow.argType}",
                  argType
                )
              )
            }
        } yield result
      }

      // Type infer for let expression
      // we need to infer the type of the variable
      // and then add it to the environment
      // and then infer the type of the body
      // and return the type of the body
      // example:
      // let x = 3 in x + 5
      // we need to infer the type of x and then add it to the environment
      // and then infer the type of x + 5
      // and return the type of x + 5
      case Binding(
            recursive: Boolean,
            variable: Variable,
            variableType: Option[Typ],
            variableAssignment: Expr,
            body: Expr
          ) => {
        for {
          inferredType <- infer(variableAssignment)
          typeCheckResult <-
            if (variableType.isDefined) {
              me.pure(Utils.equals(variableType.get, inferredType))
            } else {
              me.pure(true)
            }

          result <-
            if (typeCheckResult) {

              val enclosedEnv = env + (variable.name -> inferredType)
              infer(body)(me, enclosedEnv)
            } else {
              me.raiseError(
                CompilerError.IncorrectType(s"$variableType", inferredType)
              )
            }
        } yield result
      }

  }

}
