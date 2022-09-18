package io.jseval

import cats.syntax.all._
import cats._
import cats.implicits._
import cats.mtl._
import cats.mtl.implicits._
import cats.syntax.all.*
import scribe.Scribe

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
        variableName = tX,
        variableType = TAny,
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
        variableName = tFinal,
        variableType = TAny,
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
        variableName = tV,
        variableType = TAny,
        body = App(
          body = App(
            body = Variable(tX),
            arg = Variable(tX)
          ),
          arg = Variable(tV)
        )
      )

      val innerAbs = Abs(
        variableName = tX,
        variableType = TAny,
        body = App(
          body = Variable(tFinal),
          arg = indirect
        )
      )

      // \f. (\x. f(x x))(\x f(x x))

      Abs(
        variableName = tFinal,
        variableType = TAny,
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
      me: MonadError[F, Throwable],
      fr: Raise[F, Error],
      sc: Scribe[F],
      env: Env
  ): F[Value] =
    expr match {
      case LiteralExpr(v) => me.pure(LiteralValue(v))
      case Buildin(Arthimetric(fn, opA, opB)) => {
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

      case Grouping(op: Expr) =>
        for {
          x <- eval(op)
          y <- Value.asDouble(x)
        } yield LiteralValue(y)

      case Cond(pred, trueBranch, falseBranch) => {
        for {
          valPredExpr <- eval(pred)
          valPred <- Value.asBool(valPredExpr)
          result <- if (valPred) eval(trueBranch) else eval(falseBranch)
        } yield result
      }
      case Abs(variable, _, body) => me.pure(Closure(env, variable, body))

      case variable @ Variable(token) =>
        env.get(token) match {
          case Some(v) => me.pure(v)
          case None    => fr.raise(UnboundedName(token))
        }

      case App(bodyExpr, arg) => {
        for {
          bodyAsAvalue <-
            eval(bodyExpr)
          cls <- Value.asClosure(bodyAsAvalue)
          argValue <-
            eval(arg)
          newEnv = cls.env + (cls.varName -> argValue)
          result <- eval(cls.body)(me, fr, sc, newEnv)
          _ <- sc.info(s"""App: $bodyExpr, argExpression: $arg
            |bodyClosure = $cls
            |newEnv= $newEnv
            |argValue = $argValue
            |result = $result
            """.stripMargin)
        } yield result
      }

      // let f \x x + 5
      // recursive = 0 , var = f , body = \x = x + 5, expr = app f x

      // let sum = \x = x + 1 {

      // }

      case Binding(
            recursive: Boolean,
            variableName: Token,
            body: Expr,
            expr: Expr
          ) =>
        for {
          bodyVal <- eval(body) // enclosure
          newEnv = env + (variableName -> bodyVal)
          result <- eval(expr)(me, fr, sc, newEnv)
          _ <- sc.info(s"""
            |Binding
            |variableName = $variableName
            |bodyVal = $bodyVal
            |newEnv = $newEnv
            |result = $result
            """.stripMargin)
        } yield result
    }
}
