package io.jseval

import cats.syntax.all._
import cats._
import cats.implicits._

object TypeInfer {

  import Expression._
  import Expression.BuildinModule._
  import Expression.BuildinModule.BuildinFn._
  import Expression.ValueModule._
  import TypModule._

  type TypeEnv = Map[Token, Typ]

  object TypeError {
    sealed trait Error
    case class IncorrectType(expectedType: String, givenType: Typ) extends Error

    case class IncorrectCompareType(leftType: Typ, rightType: Typ) extends Error

    case class IncorrectArgumentType(leftType: Typ, rightType: Typ)
        extends Error

    case class UnboundedNameWhileTypeCheck(variableName: Token) extends Error
  }

  object Utils {

    // def checkEquals[F[_]](typ1: Typ, typ2: Typ)(implicit me: MonadError[F, TypeError.Error]): F[Typ] = {

    //   typ1 match {
    //     case TArrow()
    //   }

    // }

    def asArrow[F[_]](
        typ: Typ
    )(implicit me: MonadError[F, TypeError.Error]): F[TArrow] = {
      typ match {
        case x: TArrow => me.pure(x)
        case _         => me.raiseError(TypeError.IncorrectType("TArrow", typ))
      }

    }

    def appInfer[F[_]](bodyType: TArrow, argType: Typ)(implicit
        me: MonadError[F, TypeError.Error]
    ): F[Typ] = {
      if (equals(bodyType.argType, argType)) {
        me.pure(bodyType.bodyType)
      } else {
        me.raiseError(
          TypeError.IncorrectArgumentType(
            bodyType.argType,
            argType
          )
        )
      }
    }

    def asType(v: LiteralType): Typ = {
      v match {
        case x: Double  => TInt
        case _: String  => TString
        case _: Boolean => TBoolean
      }
    }

    def asInt[F[_]](
        typ: Typ
    )(implicit me: MonadError[F, TypeError.Error]): F[Typ] = {
      typ match {
        case TInt => me.pure(TInt)
        case _    => me.raiseError(TypeError.IncorrectType("TInt", typ))
      }
    }

    def asBool[F[_]](
        typ: Typ
    )(implicit me: MonadError[F, TypeError.Error]): F[Typ] = {
      typ match {
        case TBoolean => me.pure(TBoolean)
        case _        => me.raiseError(TypeError.IncorrectType("TBoolean", typ))
      }
    }

    def equals(typ1: Typ, typ2: Typ): Boolean = {
      (typ1, typ2) match {
        case (TInt, TInt)         => true
        case (TString, TString)   => true
        case (TBoolean, TBoolean) => true
        case (t1: TArrow, t2: TArrow) =>
          equals(t1.argType, t2.argType) && equals(t1.bodyType, t2.bodyType)
        case _ => false
      }
    }

  }

  def infer[F[_]](
      expr: Expr
  )(implicit me: MonadError[F, TypeError.Error], env: TypeEnv): F[Typ] =
    expr match {

      case TupleExpr(leftExpr, rightExpr) =>
        for {
          leftType <- infer(leftExpr)
          rightType <- infer(rightExpr)
        } yield TProduct(leftType, rightType)

      case LiteralExpr(v) => me.pure(Utils.asType(v))
      case Buildin(Arithmetic(fn, opA, opB)) => {
        for {
          aAsType <- infer(opA)
          aAsDouble <- Utils.asInt(aAsType)
          bAsType <- infer(opB)
          bAsDouble <- Utils.asInt(bAsType)
        } yield TInt
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
                TypeError.IncorrectCompareType(aAsValue, bAsValue)
              )
            }
        } yield TBoolean
      }

      case Buildin(Unary(fn, op)) => {
        for {
          aAsValue <- infer(op)
          aAsDouble <- Utils.asInt(aAsValue)
        } yield TInt

      }

      case Buildin(Logical(fn, opA, opB)) => {
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
                TypeError.IncorrectType(s"$trueBranchType", falseBranchType)
              )
            }
        } yield result
      }

      case Abs(variable, variableType, body) => {
        val newEnv = env + (variable.name -> variableType.getOrElse(TAny))

        infer(body)(me, newEnv).flatMap(bodyType =>
          me.pure(
            TArrow(variableType.getOrElse(TAny), bodyType)
          )
        )

      }

      case variable @ Variable(token) =>
        env.get(token) match {
          case Some(v) => me.pure(v)
          case None =>
            me.raiseError(TypeError.UnboundedNameWhileTypeCheck(token))
        }

      case App(expr, arg) => {
        for {
          bodyType <- infer(expr)
          bodyTypeAsArrow <- Utils.asArrow(bodyType)
          argValue <- infer(arg)
          result <- Utils.appInfer(bodyTypeAsArrow, argValue)

        } yield result
      }

      // let f \x x + 5
      // recursive = 0 , var = f , body = \x = x + 5, expr = app f x

      // let sum = \x = x + 1 {

      // }

      case Binding(
            recursive: Boolean,
            variableName: Variable,
            variableType: Option[Typ],
            body: Expr,
            expr: Expr
          ) =>
        for {
          bodyVal <- infer(body) // enclosure
          newEnv = env + (variableName.name -> bodyVal)
          result <- infer(expr)(me, newEnv)
        } yield result
    }
}
