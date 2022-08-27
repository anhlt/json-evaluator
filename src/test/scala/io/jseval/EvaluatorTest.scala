package io.jseval

import cats.implicits._
import munit.CatsEffectSuite
import Expression as Expr
import Expression._
import Expression.BuildinModule._
import Expression.BuildinModule.BuildinFn._
import Expression.ValueModule._
import TypModule.TInt
import Evaluator as ExprEval
import io.jseval.{Token, Literal}

class EvaluatorTest extends munit.FunSuite:

  type MyEither[A] = Either[Expr.Error, A]

  val tokenX = Literal.Identifier("x")
  val tokenY = Literal.Identifier("y")
  val tokenZ = Literal.Identifier("z")
  val tokenU = Literal.Identifier("u")

  val sumToken = Literal.Identifier("sum")
  val incToken = Literal.Identifier("inc")

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

    assertEquals(result, Right(LiteralValue(5.0)))

  }

  test("lambda x: x + 3") {

    val token = Literal.Identifier("dummy")
    val variable = Expr.Variable(token)

    // val value = Expr.LiteralValue(5)

    val bodyExpr = Abs(
      variableName = token,
      variableType = TInt,
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

    assertEquals(result, Right(LiteralValue(8.0)))

  }

  // \x \y : x + y

  // \y: x + y {x -> 5}
  // x + y {x -> 5, y -> 6}

  test("lambda \\x \\y x + y") {

    def sum(xExpression: Expr, yExpression: Expr): Expr = {
      val bodyExpr =
        Abs(
          variableName = tokenX,
          variableType = TInt,
          Abs(
            variableName = tokenY,
            variableType = TInt,
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
          App(body = bodyExpr, arg = xExpression),
          arg = yExpression
        )
      app
    }

    val result: MyEither[Value] =
      ExprEval.eval[MyEither](sum(Expr.LiteralExpr(5.0), Expr.LiteralExpr(6.0)))

    assertEquals(result, Right(LiteralValue(11.0)))

  }

  test("Binding: let inc = \\x x + 3 and inc(1) + 1") {

    // Let inc = \x x + 3 {
    //
    // }

    // define \x x + 3

    val bodyExpr = Abs(
      variableName = tokenX,
      variableType = TInt,
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

    // binding sum = \x x + 3

    def inc3(x: Double) = Binding(
      recursive = false,
      variableName = sumToken,
      body = bodyExpr,
      expr = App(
        body = Variable(sumToken),
        arg = Expr.LiteralExpr(x)
      )
    )

    val result: MyEither[Value] = ExprEval.eval[MyEither](inc3(1.0))
    assertEquals(result, Right(LiteralValue(4.0)))

  }

  test("""
    Let y = 1
    Let inc = \x x + 3
    in inc(y)
    """.stripMargin) {

    // Let inc = \x x + 3

    // define \x x + 3

    val bodyExpr = Abs(
      variableName = tokenX,
      variableType = TInt,
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

    // binding inc = \x x + 3

    val incBind = Binding(
      recursive = false,
      variableName = incToken,
      body = bodyExpr,
      expr = App(
        body = Variable(incToken),
        arg = Expr.Variable(tokenY)
      )
    )

    // let y = 1
    // let sum = \x \x + 3
    // in sum(y)

    val finalBind = Binding(
      recursive = false,
      variableName = tokenY,
      body = Expr.LiteralExpr(1.0),
      expr = incBind
    )

    val result: MyEither[Value] = ExprEval.eval[MyEither](finalBind)
    assertEquals(result, Right(LiteralValue(4.0)))

  }

  test("""
    Let z = 4
    Let t = 3
    Let sum = \x \y x + y
    in sum(z, t)
    """) {

    // \y => x + y

    val yEqualtoXplusY = Abs(
      variableName = tokenY,
      variableType = TInt,
      body = Buildin(
        BuildinFn.Arthimetric(
          BuildinFn.Add,
          x,
          y
        )
      )
    )

    // \x \y x + y

    val sumBody = Abs(
      variableName = tokenX,
      variableType = TInt,
      yEqualtoXplusY
    )

    // binding sum = \x \y x + y

    val sum = Binding(
      recursive = false,
      variableName = sumToken,
      body = sumBody,
      expr = App(
        App(body = Variable(sumToken), arg = Expr.Variable(tokenZ)),
        arg = Expr.Variable(tokenU)
      )
    )

    // Let z = 4
    // Let u = 3
    // in sum(z + u)

    val finalBind = Binding(
      recursive = false,
      variableName = tokenZ,
      body = Expr.LiteralExpr(4.0),
      expr = Binding(
        recursive = false,
        variableName = tokenU,
        body = Expr.LiteralExpr(3.0),
        expr = sum
      )
    )

    val result: MyEither[Value] = ExprEval.eval[MyEither](finalBind)
    assertEquals(result, Right(LiteralValue(7.0)))
  }
