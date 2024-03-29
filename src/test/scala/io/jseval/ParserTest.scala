package io.jseval

import cats.*
import cats.implicits.*
import Expression.*
import Expression.BuildinModule.*
import Expression.BuildinModule.BuildinFn
import Expression.ValueModule.*
import io.jseval.Scanner.*
import Keyword.*
import Operator.*
import Literal.*
import io.jseval.TypModule.*
import io.jseval.Expression.BuildinModule.BuildinFn.Arithmetic
import io.jseval.Expression.BuildinModule.BuildinFn.Subtract
import io.jseval.Expression.BuildinModule.BuildinFn.Multiply
import io.jseval.parser.{ExpressionParser, ParserOut, Precendence}

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

    assertEquals(
      ExpressionParser.parsePrecedence(Precendence.LOWEST, ts),
      Right(ParserOut(expected, Nil))
    )

  }

  test("parse_primary_string") {
    val ts = List(Literal.Str("you rox!"))
    val want = LiteralExpr("you rox!")

    assertEquals(
      ExpressionParser.parsePrecedence(Precendence.LOWEST, ts),
      Right(ParserOut(want, Nil))
    )

  }

  test("parse_unary") {
    val ts = List(
      MinusToken,
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
    assertEquals(
      ExpressionParser.parsePrecedence(Precendence.LOWEST, ts),
      Right(ParserOut(want, Nil))
    )
  }

  test("parse_factor") {
    val ts = List(
      Number(
        "5"
      ),
      StarToken,
      Number(
        "6"
      ),
      StarToken,
      MinusToken,
      Number(
        "4"
      )
    )

    val want = Buildin(
      BuildinFn.Arithmetic(
        BuildinFn.Multiply,
        Buildin(
          BuildinFn.Arithmetic(
            BuildinFn.Multiply,
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

    assertEquals(
      ExpressionParser.parsePrecedence(Precendence.LOWEST, ts),
      Right(ParserOut(want, Nil))
    )

  }

  test("parse_nested_app") {
    println("f (1, 2, g(1, 2));")
    val ts = List(
      Literal.Identifier("f"),
      Operator.LeftParenToken,
      Literal.Number("1"),
      Operator.CommaToken,
      Literal.Number("2"),
      Operator.CommaToken,
      Literal.Identifier("g"),
      Operator.LeftParenToken,
      Literal.Number("1"),
      Operator.CommaToken,
      Literal.Number("2"),
      Operator.RightParenToken,
      Operator.RightParenToken
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
    assertEquals(
      ExpressionParser.parsePrecedence(Precendence.LOWEST, ts),
      Right(ParserOut(want, Nil))
    )

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
              fn = Subtract,
              opA = Variable(Identifier("n")),
              opB = LiteralExpr(1.0)
            )
          )
        ),
        opB = App(
          body = Variable(name = Identifier("fibo")),
          arg = Buildin(fn =
            Arithmetic(
              fn = Subtract,
              opA = Variable(Identifier("n")),
              opB = LiteralExpr(2.0)
            )
          )
        )
      )
    )

    val result = for {
      tokens <- Scanner.parse(input)
      bindExpr <- ExpressionParser.parsePrecedence(Precendence.LOWEST, tokens)

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
                  fn = Subtract,
                  opA = Variable(Identifier("n")),
                  opB = LiteralExpr(1.0)
                )
              )
            ),
            opB = App(
              body = Variable(name = Identifier("fibo")),
              arg = Buildin(fn =
                Arithmetic(
                  fn = Subtract,
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
      bindExpr <- ExpressionParser.parsePrecedence(Precendence.LOWEST, tokens)

    } yield bindExpr

    assertEquals(result, Right(ParserOut(want, Nil)))

  }

  test("parse_complex_func") {

    val input = """
  fun x -> if x == 0 then 1 else x * fact(x - 1)
  """

    val xMinus1 = Buildin(
      Arithmetic(Subtract, x, LiteralExpr(1))
    )

    val falseBranch = Buildin(
      Arithmetic(Multiply, x, App(body = factorialVariable, arg = xMinus1))
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
      variableType = None,
      body = factExpr
    )

    val result = for {
      tokens <- Scanner.parse(input)
      bindExpr <- ExpressionParser.parsePrecedence(Precendence.LOWEST, tokens)

    } yield bindExpr

    assertEquals(result, Right(ParserOut(want, Nil)))

  }

  test("binding_rec") {
    val input = """
  |let rec fact: int -> int = fun x -> if x == 0 then 1 else x * fact(x - 1)
  |in fact(5)
  """.stripMargin

    val xMinus1 = Buildin(
      Arithmetic(Subtract, x, LiteralExpr(1))
    )

    val falseBranch = Buildin(
      Arithmetic(Multiply, x, App(body = factorialVariable, arg = xMinus1))
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
      variableType = None,
      body = factExpr
    )

    val want = Binding(
      recursive = true,
      variableName = factorialVariable,
      variableType = Some(TArrow(TInt, TInt)),
      body = sumBody,
      expr = App(
        body = factorialVariable,
        arg = LiteralExpr(5.0)
      )
    )

    val result = for {
      tokens <- Scanner.parse(input)
      bindExpr <- ExpressionParser.parsePrecedence(Precendence.LOWEST, tokens)

    } yield bindExpr

    assertEquals(result, Right(ParserOut(want, Nil)))

  }
