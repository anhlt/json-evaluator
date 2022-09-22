package io.jseval

import cats.implicits._
import munit.CatsEffectSuite
import Expression as Expr
import Expression._
import Expression.BuildinModule._
import Expression.BuildinModule.BuildinFn._
import Expression.ValueModule._
import TypModule._
import io.jseval.{Token, Literal}
import TypeInfer._
import TypeInfer.TypeError

class TypeInferTest extends munit.FunSuite:

  type MyEither[A] = Either[TypeError.Error, A]

  val tokenX = Literal.Identifier("x")
  val tokenY = Literal.Identifier("y")
  val tokenZ = Literal.Identifier("z")
  val tokenU = Literal.Identifier("u")

  val sumToken = Literal.Identifier("sum")
  val incToken = Literal.Identifier("inc")

  val x = Expr.Variable(tokenX)
  val y = Expr.Variable(tokenY)

  implicit val env: TypeInfer.TypeEnv = Map()

  test("simple 2 + 3 should be TInt") {

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

    val result = TypeInfer.infer[MyEither](expr)

    assertEquals(result, Right(TInt))

  }

  test(" if ('a' == 3) {1} else {2} should be Invalid") {

    val expr = Cond(
      Buildin(
        BuildinFn.Comparison(
          BuildinFn.Equal,
          opA = Expr.LiteralExpr("a"),
          opB = Expr.LiteralExpr(1)
        )
      ),
      trueBranch = Expr.LiteralExpr(1),
      falseBranch = Expr.LiteralExpr(2)
    )

    val result = TypeInfer.infer[MyEither](expr)

    val expected = TypeError.IncorrectCompareType(
      leftType = TString,
      rightType = TInt
    )

    assertEquals(result, Left(expected))

  }

  test(" if ('a' == 'b') {1} else {2} should be TInt") {

    val expr = Cond(
      Buildin(
        BuildinFn.Comparison(
          BuildinFn.Equal,
          opA = Expr.LiteralExpr("a"),
          opB = Expr.LiteralExpr("b")
        )
      ),
      trueBranch = Expr.LiteralExpr(1),
      falseBranch = Expr.LiteralExpr(2)
    )

    val result = TypeInfer.infer[MyEither](expr)

    assertEquals(result, Right(TInt))

  }

  test(" if ('a' == 'b') {1} else {'a'} should be Invalid") {

    val expr = Cond(
      Buildin(
        BuildinFn.Comparison(
          BuildinFn.Equal,
          opA = Expr.LiteralExpr("a"),
          opB = Expr.LiteralExpr("b")
        )
      ),
      trueBranch = Expr.LiteralExpr(1),
      falseBranch = Expr.LiteralExpr("a")
    )

    val result = TypeInfer.infer[MyEither](expr)

    val expected = TypeError.IncorrectType("TInt", TString)

    assertEquals(result, Left(expected))

  }

  test("""
    Let z = 4
    Let t = 3
    Let sum = \x \y x + y
    in sum(z, t)

    should be TInt
    """) {

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

    val result: MyEither[Typ] = TypeInfer.infer[MyEither](finalBind)
    assertEquals(result, Right(TInt))
  }

  test("""
    Let z = "a"
    Let t = 3
    Let sum = \x \y x + y
    in sum(z, t)

    should be Invalid
    """) {

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
        body = Expr.LiteralExpr("3.0"),
        expr = sum
      )
    )

    val result: MyEither[Typ] = TypeInfer.infer[MyEither](finalBind)

    val expected = TypeError.IncorrectArgumentType(TInt, TString)

    assertEquals(result, Left(expected))
  }
