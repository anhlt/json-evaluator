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
import io.jseval.TypModule.TAny
import cats.data._
import scala.util.{Try, Success}
import scribe.cats._
import cats.effect._
import cats.effect.implicits._
import cats.effect.unsafe.implicits.global
import scala.concurrent.ExecutionContext

class EvaluatorTest extends munit.FunSuite:

  type MyEither[A] = EitherT[IO, Expr.Error, A]

  val tokenX = Literal.Identifier("x")
  val tokenY = Literal.Identifier("y")
  val tokenZ = Literal.Identifier("z")
  val tokenU = Literal.Identifier("u")
  val tokenN = Literal.Identifier("n")
  val factorialName = Literal.Identifier("f")

  val sumToken = Literal.Identifier("sum")
  val incToken = Literal.Identifier("inc")

  val x = Expr.Variable(tokenX)
  val y = Expr.Variable(tokenY)
  val n = Expr.Variable(tokenN)
  val factorialVariable = Expr.Variable(factorialName)

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

    val value = result.value.unsafeRunSync()

    assertEquals(value, Right(LiteralValue(5.0)))

  }

  test("lambda x: x + 3") {

    val token = Literal.Identifier("dummy")
    val variable = Expr.Variable(token)

    // val value = Expr.LiteralValue(5)

    val bodyExpr = Abs(
      variableName = Variable(token),
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

    assertEquals(result.value.unsafeRunSync(), Right(LiteralValue(8.0)))

  }

  // \x \y : x + y

  // \y: x + y {x -> 5}
  // x + y {x -> 5, y -> 6}

  test("lambda \\x \\y x + y") {

    def sum(xExpression: Expr, yExpression: Expr): Expr = {
      val bodyExpr =
        Abs(
          variableName = Variable(tokenX),
          variableType = TInt,
          Abs(
            variableName = Variable(tokenY),
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

    assertEquals(result.value.unsafeRunSync(), Right(LiteralValue(11.0)))

  }

  test("Binding: let inc = \\x x + 3 and inc(1) + 1") {

    // Let inc = \x x + 3 {
    //
    // }

    // define \x x + 3

    val bodyExpr = Abs(
      variableName = Variable(tokenX),
      variableType = TInt,
      body = Buildin(
        BuildinFn.Arthimetric(
          BuildinFn.Add,
          x,
          Expr.LiteralExpr(
            3
          )
        )
      )
    )

    // binding sum = \x x + 3

    def inc3(x: Double) = Binding(
      recursive = false,
      variableName = Variable(sumToken),
      body = bodyExpr,
      expr = App(
        body = Variable(sumToken),
        arg = Expr.LiteralExpr(x)
      )
    )

    val result: MyEither[Value] = ExprEval.eval[MyEither](inc3(1.0))
    assertEquals(result.value.unsafeRunSync(), Right(LiteralValue(4.0)))

  }

  test("""
    Let y = 1
    Let inc = \x x + 3
    in inc(y)
    """.stripMargin) {

    // Let inc = \x x + 3

    // define \x x + 3

    val bodyExpr = Abs(
      variableName = Variable(tokenX),
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
      variableName = Variable(incToken),
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
      variableName = Variable(tokenY),
      body = Expr.LiteralExpr(1.0),
      expr = incBind
    )

    val result: MyEither[Value] = ExprEval.eval[MyEither](finalBind)
    assertEquals(result.value.unsafeRunSync(), Right(LiteralValue(4.0)))

  }

  test("sum_two_numbers") {

    // \y => x + y

    val yEqualtoXplusY = Abs(
      variableName = Variable(tokenY),
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
      variableName = Variable(tokenX),
      variableType = TInt,
      yEqualtoXplusY
    )

    // binding sum = \x \y x + y

    val sum = Binding(
      recursive = false,
      variableName = Variable(sumToken),
      body = sumBody,
      expr = App(
        App(body = Variable(sumToken), arg = Expr.Variable(tokenZ)),
        arg = Expr.Variable(tokenU)
      )
    )

    // Let z = 4
    // Let u = 3
    // Let sum = x y -> x + y
    // in sum(z , u)

    val finalBind = Binding(
      recursive = false,
      variableName = Variable(tokenZ),
      body = LiteralExpr(4.0),
      expr = Binding(
        recursive = false,
        variableName = Variable(tokenU),
        body = Expr.LiteralExpr(3.0),
        expr = sum
      )
    )

    val result: MyEither[Value] = ExprEval.eval[MyEither](finalBind)
    assertEquals(result.value.unsafeRunSync(), Right(LiteralValue(7.0)))
  }

  test("""Factorial""") {

    // x - 1

    val xMinus1 = Buildin(
      Arthimetric(Sub, x, LiteralExpr(1))
    )

    // x * factorial(x-1)

    val falseBranch = Buildin(
      Arthimetric(Mul, x, App(body = factorialVariable, arg = xMinus1))
    )

    // x == 0
    val comparision = Buildin(
      Comparison(Equal, x, LiteralExpr(0))
    )

    // 1 if x == 0 else x * factorial(x - 1)

    val factExpr = Cond(
      pred = comparision,
      trueBranch = LiteralExpr(1),
      falseBranch = falseBranch
    )

    // body = \factoria \x 1 if x == 0 else x * factorial(x - 1)

    val body = Abs(
      variableName = Variable(factorialName),
      variableType = TAny,
      body = Abs(
        variableName = Variable(tokenX),
        variableType = TAny,
        body = factExpr
      )
    )

    val lfp = ExprEval.Utils.eagerFixPoint

    val facApp = App(body = lfp, arg = body)

    val binding = Binding(
      recursive = true,
      variableName = Variable(factorialName),
      body = facApp,
      expr = App(
        body = factorialVariable, //
        arg = LiteralExpr(5)
      )
    )

    val result: MyEither[Value] = ExprEval.eval[MyEither](binding)
    assertEquals(result.value.unsafeRunSync(), Right(LiteralValue(120)))

  }
