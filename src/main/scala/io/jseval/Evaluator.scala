package io.jseval

import cats.syntax.all._
import cats._
import cats.implicits._

object Evaluator {

  import Expression._
  import BuildinFn._

  def eval[F[_]](
      expr: Expr
  )(implicit me: MonadError[F, Error], env: Env): F[Value] =
    expr match {
      case LiteralExpr(v) => me.pure(LiteralValue(v))
      case Buildin(Arthimetric(fn, opA, opB)) => {
        for {
          valA1 <- eval(opA)
          valA <- Value.asDouble(valA1)
          valB1 <- eval(opB)
          valB <- Value.asDouble(valB1)
        } yield LiteralValue(ArthimetricFn.apply(fn)(valA)(valB))
      }

      case Buildin(Comparison(fn, opA, opB)) => {
        for {
          valA <- eval(opA) flatMap Value.asDouble
          valB <- eval(opB) flatMap Value.asDouble
        } yield LiteralValue(ComparisonFn.apply(fn)(valA)(valB))
      }

      case Buildin(Unary(fn, op)) => {
        for {
          valA1 <- eval(op)
          valA <- Value.asDouble(valA1)
        } yield LiteralValue(UnaryFn.apply(fn)(valA))

      }

      case Buildin(Logical(fn, opA, opB)) => {
        for {
          valA1 <- eval(opA)
          valA <- Value.asDouble(valA1)
          valB1 <- eval(opB)
          valB <- Value.asDouble(valB1)
        } yield LiteralValue(LogicalFn.apply(fn)(valA)(valB))
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
      case Abs(variable, body) => me.pure(Closure(env, variable, body))

      case variable @ Variable(token) =>
        env.get(token) match {
          case Some(v) => me.pure(v)
          case None    => me.raiseError(UnboundedName(token))
        }

      case App(expr, arg) => {
        for {
          closureValue <- eval(expr)
          cls <- Value.asClosure(closureValue)
          argValue <- eval(arg)
          newEnv = cls.env + (cls.varName -> argValue)
          result <- eval(cls.body)(me, newEnv)
        } yield result
      }

      // let f x = x + 5
      // recursive = 0 , var = f , body = \x = x + 5, expr = app f x

      case Binding(
            recursive: Boolean,
            variable: Variable,
            body: Expr,
            expr: Expr
          ) =>
        for {
          bodyVal <- eval(body) // enclosure
          newEnv = env + (variable.name -> bodyVal)
          result <- eval(expr)(me, newEnv)
        } yield result
    }
}
