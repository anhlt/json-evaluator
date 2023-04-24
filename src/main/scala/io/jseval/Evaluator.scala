package io.jseval

import cats._
import cats.implicits._
import cats.mtl._
import cats.mtl.implicits._

object Evaluator {

  import Expression._
  import Expression.BuildinModule._
  import Expression.BuildinModule.BuildinFn._
  import Expression.ValueModule._

  import TypModule._

  object Utils {

    val lazyFixPoint = {

      val tX = Literal.Identifier("x")
      val tFinal = Literal.Identifier("final")

      // \x. f(x x)

      val innerAbs = Abs(
        variableName = Variable(tX),
        variableType = None,
        body = App(
          body = App(
            body = Variable(tX),
            arg = Variable(tX)
          ),
          arg = Variable(tFinal)
        )
      )

      // \f. (\x. f(x x))(\x f(x x))

      Abs(
        variableName = Variable(tFinal),
        variableType = None,
        body = App(
          body = innerAbs,
          arg = innerAbs
        )
      )
    }

    val eagerFixPoint = {

      val tX = Literal.Identifier("x")
      val tV = Literal.Identifier("v")
      val tFinal = Literal.Identifier("f")

      // \x. f(x x)

      val indirect = Abs(
        variableName = Variable(tV),
        variableType = None,
        body = App(
          body = App(
            body = Variable(tX),
            arg = Variable(tX)
          ),
          arg = Variable(tV)
        )
      )

      val innerAbs = Abs(
        variableName = Variable(tX),
        variableType = None,
        body = App(
          body = Variable(tFinal),
          arg = indirect
        )
      )

      // \f. (\x. f(x x))(\x f(x x))

      Abs(
        variableName = Variable(tFinal),
        variableType = None,
        body = App(
          body = innerAbs,
          arg = innerAbs
        )
      )
    }

  }

  def eval[F[_]](
      expr: Expr
  )(implicit
      me: MonadError[F, CompilerError], //
      env: Env
  ): F[Value] =
    expr match {
      case LiteralExpr(v) => me.pure(LiteralValue(v))
      case Buildin(Arithmetic(fn, opA, opB)) => {
        for {
          aAsValue <- eval(opA)
          aAsDouble <- Value.asDouble(aAsValue)
          bAsValue <- eval(opB)
          bAsDouble <- Value.asDouble(bAsValue)
        } yield LiteralValue(ArthimetricFn.apply(fn)(aAsDouble)(bAsDouble))
      }

      case Buildin(Comparison(fn, opA, opB)) => {
        for {
          aAsValue <- eval(opA)
          aAsDouble <- Value.asDouble(aAsValue)
          bAsValue <- eval(opB)
          bAsDouble <- Value.asDouble(bAsValue)
        } yield LiteralValue(ComparisonFn.apply(fn)(aAsDouble)(bAsDouble))
      }

      case Buildin(Unary(fn, op)) => {
        for {
          aAsValue <- eval(op)
          aAsDouble <- Value.asDouble(aAsValue)
        } yield LiteralValue(UnaryFn.apply(fn)(aAsDouble))

      }

      case Buildin(Logical(fn, opA, opB)) => {
        for {
          aAsValue <- eval(opA)
          aAsDouble <- Value.asDouble(aAsValue)
          bAsValue <- eval(opB)
          bAsDouble <- Value.asDouble(bAsValue)
        } yield LiteralValue(LogicalFn.apply(fn)(aAsDouble)(bAsDouble))
      }

      case Cond(pred, trueBranch, falseBranch) => {
        for {
          valPredExpr <- eval(pred)
          valPred <- Value.asBool(valPredExpr)
          result <- if (valPred) eval(trueBranch) else eval(falseBranch)
        } yield result
      }
      case Abs(variable, _, body) => me.pure(Closure(env, variable.name, body))

      case variable @ Variable(token) =>
        env.get(token) match {
          case Some(v) => me.pure(v)
          case None    => me.raiseError(CompilerError.UnboundedName(token))
        }

      case App(bodyExpr, arg) => {
        for {
          bodyAsAvalue <-
            eval(bodyExpr)
          cls <- Value.asClosure(bodyAsAvalue)
          argValue <-
            eval(arg)
          newEnv = cls.env + (cls.varName -> argValue)
          result <- eval(cls.body)(me, newEnv)
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
            expr: Expr,
          ) =>
        if (recursive) {

          val fixRecusive = App(
            body = Utils.eagerFixPoint,
            arg =
              Abs(variableName = variableName, variableType = None, body = body)
          )

          for {
            bodyVal <- eval(fixRecusive) // enclosure
            newEnv = env + (variableName.name -> bodyVal)
            result <- eval(expr)(me, newEnv)
          } yield result

        } else
          for {
            bodyVal <- eval(body) // enclosure
            newEnv = env + (variableName.name -> bodyVal)
            result <- eval(expr)(me, newEnv)
          } yield result
    }
}
