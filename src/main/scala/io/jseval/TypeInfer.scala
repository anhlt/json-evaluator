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
    case class IncorrectType(expectedType: String, givenType: String)
        extends Error

    case class UnboundedNameWhileTypeCheck(variableName: Token) extends Error
  }

  object Utils {

    // def checkEquals[F[_]](typ1: Typ, typ2: Typ)(implicit me: MonadError[F, TypeError.Error]): F[Typ] = {

    //   typ1 match {
    //     case TArrow()
    //   }

    // }

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
        case _    => me.raiseError(TypeError.IncorrectType("TInt", ""))
      }
    }

    def asBool[F[_]](
        typ: Typ
    )(implicit me: MonadError[F, TypeError.Error]): F[Typ] = {
      typ match {
        case TInt => me.pure(TInt)
        case _    => me.raiseError(TypeError.IncorrectType("TInt", ""))
      }
    }

    def equals(typ1: Typ, typ2: Typ): Boolean = {
      (typ1, typ2) match {
        case (_: TInt, _: TInt)         => True
        case (_: TString, _: TString)   => True
        case (_: TBoolean, _: TBoolean) => True
        case (t1: TArrow, t2: TArrow) =>
          equals(t1.argType, t2.argType) && equals(t1.bodyType, t.bodyType)
      }
    }

  }

  def infer[F[_]](
      expr: Expr
  )(implicit me: MonadError[F, TypeError.Error], env: TypeEnv): F[Typ] =
    expr match {
      case LiteralExpr(v) => me.pure(Utils.asType(v))
      case Buildin(Arthimetric(fn, opA, opB)) => {
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
          aAsDouble <- Utils.asInt(aAsValue)
          bAsValue <- infer(opB)
          bAsDouble <- Utils.asInt(bAsValue)
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

      case Grouping(op: Expr) =>
        for {
          x <- infer(op)
        } yield x

      // case Cond(pred, trueBranch, falseBranch) => {
      //   for {
      //     valPredExpr <- eval(pred)
      //     valPred <- Value.asBool(valPredExpr)
      //     result <- if (valPred) eval(trueBranch) else eval(falseBranch)
      //   } yield result
      // }

      case Abs(variable, variableType, body) => {
        val newEnv = env + (variable -> variableType)

        infer(body)(me, newEnv).flatMap(bodyType =>
          me.pure(
            TArrow(variableType, bodyType)
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
          bodyAsAvalue <- infer(expr)
          cls <- Value.asClosure(bodyAsAvalue)
          argValue <- infer(arg)
          newEnv = cls.env + (cls.varName -> argValue)
          result <- infer(cls.body)(me, newEnv)
        } yield result
      }

      // let f \x x + 5
      // recursive = 0 , var = f , body = \x = x + 5, expr = app f x

      // let sum = \x = x + 1 {

      // }

      // case Binding(
      //       recursive: Boolean,
      //       variableName: Token,
      //       body: Expr,
      //       expr: Expr
      //     ) =>
      //   for {
      //     bodyVal <- eval(body) // enclosure
      //     newEnv = env + (variableName -> bodyVal)
      //     result <- eval(expr)(me, newEnv)
      //   } yield result
    }

}
