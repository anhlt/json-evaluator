package io.jseval

import munit.CatsEffectSuite
import Expression as Expr
import Expression._
import Expression.BuildinModule._
import Expression.BuildinModule.BuildinFn._
import Expression.ValueModule._
import TypModule.{TInt, TAny}
import Evaluator as ExprEval
import io.jseval.{Token, Literal}
import cats.data._
import cats.effect._
import cats.effect.implicits._
import cats.implicits._
import io.jseval.parser.ExpressionParser

class EvaluatorTest extends munit.FunSuite:

  // type MyEither[A] = EitherT[IO, Error, A]
  type MyEither[A] = Either[CompilerError, A]

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

  test("evaluate_simple_expression") {

    val expr = Buildin(
      BuildinFn.Arithmetic(
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

  test("evaluate_lambda") {

    val token = Literal.Identifier("dummy")
    val variable = Expr.Variable(token)

    // val value = Expr.LiteralValue(5)

    val bodyExpr = Abs(
      variableName = Variable(token),
      variableType = TInt,
      body = Buildin(
        BuildinFn.Arithmetic(
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

  test("evaluate_nested_lambda") {

    def sum(xExpression: Expr, yExpression: Expr): Expr = {
      val bodyExpr =
        Abs(
          variableName = Variable(tokenX),
          variableType = TInt,
          Abs(
            variableName = Variable(tokenY),
            variableType = TInt,
            body = Buildin(
              BuildinFn.Arithmetic(
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
      variableName = Variable(tokenX),
      variableType = TInt,
      body = Buildin(
        BuildinFn.Arithmetic(
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
    assertEquals(result, Right(LiteralValue(4.0)))

  }

  test("evaluate_inc_function") {

    // Let inc = \x x + 3

    // define \x x + 3

    val bodyExpr = Abs(
      variableName = Variable(tokenX),
      variableType = TInt,
      body = Buildin(
        BuildinFn.Arithmetic(
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
    assertEquals(result, Right(LiteralValue(4.0)))

  }

  test("evaluate_sum_two_numbers") {

    // \y => x + y

    val yEqualtoXplusY = Abs(
      variableName = Variable(tokenY),
      variableType = TInt,
      body = Buildin(
        BuildinFn.Arithmetic(
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
    assertEquals(result, Right(LiteralValue(7.0)))
  }

  test("""evaluate_earge_fix_point""") {

    // x - 1

    val xMinus1 = Buildin(
      Arithmetic(Sub, x, LiteralExpr(1))
    )

    // x * factorial(x-1)

    val falseBranch = Buildin(
      Arithmetic(Mul, x, App(body = factorialVariable, arg = xMinus1))
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
    assertEquals(result, Right(LiteralValue(120)))

  }

  test("evaluate_complex_input") {

    val input = """
   |let mul = fun x y -> x * y
   |let sum = fun x y -> x + y
   |let x = 5
   |let y = 6
   |in sum(12, mul(x, y))
   """.stripMargin

    val parserResult = for {
      tokens <- Scanner.parse(input)
      bindExpr <- ExpressionParser.expression(tokens)
      value <- ExprEval.eval(bindExpr.expr)

    } yield bindExpr

    val a = parserResult.map(_.expr).getOrElse(LiteralExpr(5))

    val result: MyEither[Value] = ExprEval.eval[MyEither](a)

    assertEquals(result, Right(LiteralValue(42)))

  }

  test("evaluate_rec_binding") {

    val input = """
   |let rec fact = fun x -> if x == 0 then 1 else x * fact(x - 1)
   |in fact(5)
   """.stripMargin

    val parserResult = for {
      tokens <- Scanner.parse(input)
      bindExpr <- ExpressionParser.expression(tokens)
      value <- ExprEval.eval(bindExpr.expr)

    } yield value

    assertEquals(parserResult, Right(LiteralValue(120)))

  }

  test("evaluate_rec_fibonacy") {

    val input = """
   |let rec fibo = fun n -> if n <= 0 then 0 else if n == 1 then 1 else fibo(n-1) + fibo(n-2)
   |in fibo(10)
   """.stripMargin

    val parserResult = for {
      tokens <- Scanner.parse(input)
      bindExpr <- ExpressionParser.expression(tokens)
      value <- ExprEval.eval(bindExpr.expr)

    } yield value

    assertEquals(parserResult, Right(LiteralValue(55)))

  }

  test("evaluate_complex_input_2") {

    val input = """
   |let mul = fun x y -> x * y
   |let sum = fun x y -> x + y
   |let x = 5
   |let y = 6
   |let z = mul(4)
   |in sum(12, mul(x, y)) + z(3)
   """.stripMargin

    val parserResult = for {
      tokens <- Scanner.parse(input)
      bindExpr <- ExpressionParser.expression(tokens)
      value <- ExprEval.eval(bindExpr.expr)

    } yield value 

    assertEquals(parserResult, Right(LiteralValue(54)))

  }
