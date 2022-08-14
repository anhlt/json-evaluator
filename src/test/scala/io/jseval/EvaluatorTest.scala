package io.jseval

import cats.implicits._
import munit.CatsEffectSuite
import Expression as Expr
import Expression._
import Evaluator as ExprEval
import io.jseval.{Token, Literal}

class EvaluatorTest extends munit.FunSuite:

  type MyEither[A] = Either[Expr.Error, A]

  val tokenX = Literal.Identifier("x")
  val tokenY = Literal.Identifier("y")
  val sumToken = Literal.Identifier("sum")

  val x = Expr.Variable(tokenX)
  val y = Expr.Variable(tokenY)

  implicit val env: Expr.Env = Map()

  test("simple 2 + 3 should be 5") {

    val expr = Buildin(
      BuildinFn.Arthimetric(
        BuildinFn.Add,
        Expr.LiteralExpr(
          2.0
        ),
        Expr.LiteralExpr(
          3.0
        )
      )
    )

    val result = ExprEval.eval[MyEither](expr)

    assertEquals(result, Right(Expr.LiteralValue(5.0)))

  }

  test("lambda x: x + 3") {

    val token = Literal.Identifier("dummy")
    val variable = Expr.Variable(token)

    // val value = Expr.LiteralValue(5)

    val bodyExpr = Abs(
      variableName = token,
      body = Buildin(
        BuildinFn.Arthimetric(
          BuildinFn.Add,
          variable,
          Expr.LiteralExpr(
            3.0
          )
        )
      )
    )

    val app = App(bodyExpr, Expr.LiteralExpr(5.0))

    val result = ExprEval.eval[MyEither](app)

    assertEquals(result, Right(Expr.LiteralValue(8.0)))

  }

  // \x \y : x + y

  // \y: x + y {x -> 5}
  // x + y {x -> 5, y -> 6}

  test("lambda \\x \\y x + y") {

    def sum(xValue: Double, yValue: Double): Expr = {
      val bodyExpr =
        Abs(
          variableName = tokenX,
          Abs(
            variableName = tokenY,
            body = Buildin(
              BuildinFn.Arthimetric(
                BuildinFn.Add,
                x,
                y
              )
            )
          )
        )

      val app =
        App(
          App(body = bodyExpr, arg = Expr.LiteralExpr(xValue)),
          arg = Expr.LiteralExpr(yValue)
        )
      app
    }

    val result: MyEither[Value] = ExprEval.eval[MyEither](sum(5.0, 6.0))

    assertEquals(result, Right(Expr.LiteralValue(11.0)))

  }

  test("Binding: let inc = \\x x + 3 and inc(1) + 1") {

    val bodyExpr = Abs(
      variableName = tokenX,
      body = Buildin(
        BuildinFn.Arthimetric(
          BuildinFn.Add,
          x,
          Expr.LiteralExpr(
            3.0
          )
        )
      )
    )

    // val addExpr = 

    val bind = Binding(
      recursive = false,
      variableName = sumToken,
      body = bodyExpr,
      expr = App(
        body = Variable(sumToken),
        arg = Expr.LiteralExpr(1.0)
      )
    )

    val result: MyEither[Value] = ExprEval.eval[MyEither](bind)
    assertEquals(result, Right(Expr.LiteralValue(4.0)))

  }
