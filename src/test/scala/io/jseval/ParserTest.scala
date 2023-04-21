package io.jseval

import Parser.*
import cats._
import cats.implicits._
import Expression._
import Expression.BuildinModule._
import Expression.BuildinModule.BuildinFn
import Expression.ValueModule._
import io.jseval.Scanner._
import Keyword._
import Operator._
import Literal._
import io.jseval.TypModule._
import io.jseval.Expression.BuildinModule.BuildinFn.Arithmetic
import io.jseval.Expression.BuildinModule.BuildinFn.Sub
import io.jseval.Expression.BuildinModule.BuildinFn.Mul

class ParserTest extends munit.FunSuite:

  val tokenX = Literal.Identifier("x")
  val tokenY = Literal.Identifier("y")
  val tokenZ = Literal.Identifier("z")
  val tokenU = Literal.Identifier("u")
  val tokenN = Literal.Identifier("n")
  val factorialName = Literal.Identifier("fact")

  val sumToken = Literal.Identifier("sum")

  val x = Variable(tokenX)
  val y = Variable(tokenY)
  val factorialVariable = Variable(factorialName)

  test(s"parse_primary_number") {
    val ts = List(Literal.Number("42"))
    val expected = Expression.LiteralExpr(42)

    assertEquals(expression(ts), Right(ParserOut(expected, Nil)))

  }

  test("parse_primary_string") {
    val ts = List(Literal.Str("you rox!"))
    val want = LiteralExpr("you rox!")

    assertEquals(expression(ts), Right(ParserOut(want, Nil)))

  }

  test("parse_unary") {
    val ts = List(
      Minus,
      Number(
        "3"
      )
    )
    val want = Buildin(
      BuildinFn.Unary(
        BuildinFn.Negate,
        LiteralExpr(
          3.0
        )
      )
    )
    assertEquals(expression(ts), Right(ParserOut(want, Nil)))
  }

  test("parse_factor") {
    val ts = List(
      Number(
        "5"
      ),
      Star,
      Number(
        "6"
      ),
      Star,
      Minus,
      Number(
        "4"
      )
    )

    val want = Buildin(
      BuildinFn.Arithmetic(
        BuildinFn.Mul,
        Buildin(
          BuildinFn.Arithmetic(
            BuildinFn.Mul,
            LiteralExpr(
              5.0
            ),
            LiteralExpr(
              6.0
            )
          )
        ),
        Buildin(
          BuildinFn.Unary(
            BuildinFn.Negate,
            LiteralExpr(
              4.0
            )
          )
        )
      )
    )

    assertEquals(expression(ts), Right(ParserOut(want, Nil)))

  }

  test("parse_abs") {
    println("fun x y -> x + y + 8;")
    val ts = List(
      Keyword.Fun,
      Literal.Identifier("x"),
      Literal.Identifier("y"),
      Operator.Arrow,
      Literal.Identifier("x"),
      Plus,
      Literal.Identifier("y"),
      Plus,
      Number(
        "8"
      )
    )

    val body = Buildin(
      BuildinFn.Arithmetic(
        BuildinFn.Add,
        Buildin(
          BuildinFn.Arithmetic(
            BuildinFn.Add,
            Variable(Identifier("x")),
            Variable(Identifier("y"))
          )
        ),
        LiteralExpr(
          8.0
        )
      )
    )

    val want = Abs(
      variableName = Variable(Identifier("x")),
      variableType = TAny,
      body = Abs(
        variableName = Variable(Identifier("y")),
        variableType = TAny,
        body = body
      )
    )
    assertEquals(lambdaFunc(ts), Right(ParserOut(want, Nil)))
  }

  test("parse_app") {
    println("x (1, 2, b)")
    val ts = List(
      Literal.Identifier("x"),
      Operator.LeftParen,
      Literal.Number("1"),
      Operator.Comma,
      Literal.Number("2"),
      Operator.Comma,
      Literal.Identifier("b"),
      Operator.RightParen
    )

    val want = App(
      App(App(Variable(Identifier("x")), LiteralExpr(1.0)), LiteralExpr(2.0)),
      Variable(Identifier("b"))
    )
    assertEquals(app(ts), Right(ParserOut(want, Nil)))

  }

  test("parse_nested_app") {
    println("f (1, 2, g(1, 2));")
    val ts = List(
      Literal.Identifier("f"),
      Operator.LeftParen,
      Literal.Number("1"),
      Operator.Comma,
      Literal.Number("2"),
      Operator.Comma,
      Literal.Identifier("g"),
      Operator.LeftParen,
      Literal.Number("1"),
      Operator.Comma,
      Literal.Number("2"),
      Operator.RightParen,
      Operator.RightParen
    )

    val want = App(
      App(App(Variable(Identifier("f")), LiteralExpr(1.0)), LiteralExpr(2.0)),
      App(
        body = App(
          body = Variable(
            name = Identifier(
              lexeme = "g"
            )
          ),
          arg = LiteralExpr(
            value = 1.0
          )
        ),
        arg = LiteralExpr(
          value = 2.0
        )
      )
    )
    assertEquals(expression(ts), Right(ParserOut(want, Nil)))

  }

  test("parse_binding") {
    val input = """
      |let z = 4
      |let u = 3
      |let sum = fun x y -> x + y
      |in sum(z, u)
      """.stripMargin

    val tokens = Scanner.parse(input)

    val result = for {
      tokens <- Scanner.parse(input)
      bindExpr <- let(tokens)

    } yield bindExpr

    val yEqualtoXplusY = Abs(
      variableName = Variable(tokenY),
      variableType = TAny,
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
      variableType = TAny,
      yEqualtoXplusY
    )

    // binding sum = \x \y x + y

    val sum = Binding(
      recursive = false,
      variableName = Variable(sumToken),
      body = sumBody,
      expr = App(
        App(body = Variable(sumToken), arg = Variable(tokenZ)),
        arg = Variable(tokenU)
      )
    )

    val want = Binding(
      recursive = false,
      variableName = Variable(tokenZ),
      body = LiteralExpr(4.0),
      expr = Binding(
        recursive = false,
        variableName = Variable(tokenU),
        body = LiteralExpr(3.0),
        expr = sum
      )
    )

    assertEquals(result, Right(ParserOut(want, Nil)))
  }

  test("parse_condition") {
    val input = """
      if (4 > 5) and true then 1 else 2
    """

    val want = Cond(
      pred = Buildin(
        fn = BuildinFn.Logical(
          fn = BuildinFn.And,
          opA = Buildin(
            fn = BuildinFn.Comparison(
              fn = BuildinFn.Greater,
              opA = LiteralExpr(
                value = 4.0
              ),
              opB = LiteralExpr(
                value = 5.0
              )
            )
          ),
          opB = LiteralExpr(
            value = true
          )
        )
      ),
      trueBranch = LiteralExpr(
        value = 1.0
      ),
      falseBranch = LiteralExpr(
        value = 2.0
      )
    )

    val result = for {
      tokens <- Scanner.parse(input)
      bindExpr <- expression(tokens)

    } yield bindExpr

    assertEquals(result, Right(ParserOut(want, Nil)))

  }

  test("parse_adding_2_function") {
    val input = "fibo(n-1) + fibo(n-2)"

    val want = Buildin(
      fn = Arithmetic(
        fn = BuildinFn.Add,
        opA = App(
          body = Variable(name = Identifier("fibo")),
          arg = Buildin(fn =
            Arithmetic(
              fn = Sub,
              opA = Variable(Identifier("n")),
              opB = LiteralExpr(1.0)
            )
          )
        ),
        opB = App(
          body = Variable(name = Identifier("fibo")),
          arg = Buildin(fn =
            Arithmetic(
              fn = Sub,
              opA = Variable(Identifier("n")),
              opB = LiteralExpr(2.0)
            )
          )
        )
      )
    )

    val result = for {
      tokens <- Scanner.parse(input)
      bindExpr <- expression(tokens)

    } yield bindExpr

    assertEquals(result, Right(ParserOut(want, Nil)))
  }

  test("parse_nested_condition") {

    val input = """
    if n <= 0
      then
        0
      else
        if n == 1 then 1 else fibo(n-1) + fibo(n-2)
  """

    val want = Cond(
      pred = Buildin(
        fn = BuildinFn.Comparison(
          fn = BuildinFn.LessEqual,
          opA = Variable(
            name = Identifier(
              "n"
            )
          ),
          opB = LiteralExpr(
            value = 0.0
          )
        )
      ),
      trueBranch = LiteralExpr(
        value = 0
      ),
      falseBranch = Cond(
        pred = Buildin(
          fn = BuildinFn.Comparison(
            fn = BuildinFn.Equal,
            opA = Variable(
              name = Identifier(
                "n"
              )
            ),
            opB = LiteralExpr(
              value = 1.0
            )
          )
        ),
        trueBranch = LiteralExpr(
          value = 1
        ),
        falseBranch = Buildin(
          fn = Arithmetic(
            fn = BuildinFn.Add,
            opA = App(
              body = Variable(name = Identifier("fibo")),
              arg = Buildin(fn =
                Arithmetic(
                  fn = Sub,
                  opA = Variable(Identifier("n")),
                  opB = LiteralExpr(1.0)
                )
              )
            ),
            opB = App(
              body = Variable(name = Identifier("fibo")),
              arg = Buildin(fn =
                Arithmetic(
                  fn = Sub,
                  opA = Variable(Identifier("n")),
                  opB = LiteralExpr(2.0)
                )
              )
            )
          )
        )
      )
    )

    val result = for {
      tokens <- Scanner.parse(input)
      bindExpr <- expression(tokens)

    } yield bindExpr

    assertEquals(result, Right(ParserOut(want, Nil)))

  }

  test("parse_complex_func") {

    val input = """
  fun x -> if x == 0 then 1 else x * fact(x - 1)
  """

    val xMinus1 = Buildin(
      Arithmetic(Sub, x, LiteralExpr(1))
    )

    val falseBranch = Buildin(
      Arithmetic(Mul, x, App(body = factorialVariable, arg = xMinus1))
    )

    val comparision = Buildin(
      BuildinFn.Comparison(BuildinFn.Equal, x, LiteralExpr(0))
    )

    val factExpr = Cond(
      pred = comparision,
      trueBranch = LiteralExpr(1),
      falseBranch = falseBranch
    )

    val want = Abs(
      variableName = Variable(tokenX),
      variableType = TAny,
      body = factExpr
    )

    val result = for {
      tokens <- Scanner.parse(input)
      bindExpr <- expression(tokens)

    } yield bindExpr

    assertEquals(result, Right(ParserOut(want, Nil)))

  }

  test("binding_rec") {
    val input = """
  |let rec fact = fun x -> if x == 0 then 1 else x * fact(x - 1)
  |in fact(5)
  """.stripMargin

    val xMinus1 = Buildin(
      Arithmetic(Sub, x, LiteralExpr(1))
    )

    val falseBranch = Buildin(
      Arithmetic(Mul, x, App(body = factorialVariable, arg = xMinus1))
    )

    val comparision = Buildin(
      BuildinFn.Comparison(BuildinFn.Equal, x, LiteralExpr(0))
    )

    val factExpr = Cond(
      pred = comparision,
      trueBranch = LiteralExpr(1),
      falseBranch = falseBranch
    )

    val sumBody = Abs(
      variableName = Variable(tokenX),
      variableType = TAny,
      body = factExpr
    )

    val want = Binding(
      recursive = true,
      variableName = factorialVariable,
      body = sumBody,
      expr = App(
        body = factorialVariable,
        arg = LiteralExpr(5.0)
      )
    )

    val result = for {
      tokens <- Scanner.parse(input)
      bindExpr <- expression(tokens)

    } yield bindExpr

    assertEquals(result, Right(ParserOut(want, Nil)))

  }
