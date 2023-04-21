package io.jseval

import Parser.*
import cats.*
import cats.implicits.*
import Expression.*
import Expression.BuildinModule.*
import Expression.BuildinModule.BuildinFn
import Expression.ValueModule.*
import Keyword.*
import Operator.*
import Literal.*
import io.jseval.Expression.BuildinModule.BuildinFn.Arithmetic
import io.jseval.Expression.BuildinModule.BuildinFn.Sub
import io.jseval.Expression.BuildinModule.BuildinFn.Mul
import io.jseval.parser.{JSParser, Precendence}
import io.jseval.Expression.BuildinModule.BuildinFn.UnaryFn
import io.jseval.TypModule.TAny
import io.jseval.Expression.BuildinModule.BuildinFn.Logical
import io.jseval.Expression.BuildinModule.BuildinFn.LogicalFn

class NewParserTest extends munit.FunSuite:

  val tokenA = Literal.Identifier("a")
  val tokenB = Literal.Identifier("b")
  val tokenC = Literal.Identifier("c")
  val tokenX = Literal.Identifier("x")
  val tokenY = Literal.Identifier("y")
  val tokenZ = Literal.Identifier("z")
  val tokenU = Literal.Identifier("u")
  val tokenN = Literal.Identifier("n")
  val tokenF = Literal.Identifier("f")
  val tokenFact = Literal.Identifier("fact")

  val tokenSum = Literal.Identifier("sum")

  /*
  while test for parsing Literal and AND OR oprator for JSParser
   */

  test(s"parse_primary_number") {

    val tokens = List(Number("1.0"))
    val parserOut = JSParser().parsePrecedence(Precendence.LOWEST, tokens)
    assertEquals(parserOut, Right(ParserOut(LiteralExpr(1.0), List())))
  }

  test(s"parse_primary_string") {
    val tokens = List(Str("hello"))
    val parserOut = JSParser().parsePrecedence(Precendence.LOWEST, tokens)
    assertEquals(parserOut, Right(ParserOut(LiteralExpr("hello"), List())))
  }

  test(s"parse_primary_identifier") {
    val tokens = List(tokenX)
    val parserOut = JSParser().parsePrecedence(Precendence.LOWEST, tokens)
    assertEquals(parserOut, Right(ParserOut(Variable(tokenX), List())))
  }

  test(s"parse_primary_boolean") {
    val tokens = List(True)
    val parserOut = JSParser().parsePrecedence(Precendence.LOWEST, tokens)
    assertEquals(parserOut, Right(ParserOut(LiteralExpr(true), List())))
  }

  //   test for parsing AND OR operator for JSParser
  test(s"parse_primary_boolean_and") {
    val tokens = List(True, And, False)
    val parserOut = JSParser().parsePrecedence(Precendence.LOWEST, tokens)

    val expected = Buildin(
      BuildinFn.Logical(
        BuildinFn.And,
        LiteralExpr(true),
        LiteralExpr(false)
      )
    )

    assertEquals(parserOut, Right(ParserOut(expected, List())))
  }

  //   test combination of And and Or precedence
  test(s"parse_primary_boolean_and_or") {
    val tokens = List(True, And, False, Or, True)
    val parserOut = JSParser().parsePrecedence(Precendence.LOWEST, tokens)

    val expected = Buildin(
      BuildinFn.Logical(
        BuildinFn.Or,
        Buildin(
          BuildinFn.Logical(
            BuildinFn.And,
            LiteralExpr(true),
            LiteralExpr(false)
          )
        ),
        LiteralExpr(true)
      )
    )

    assertEquals(parserOut, Right(ParserOut(expected, List())))
  }

  // test combinataion of Or and And precedence
  test(s"parse_primary_boolean_or_and") {
    val tokens = List(True, Or, False, And, True)
    val parserOut = JSParser().parsePrecedence(Precendence.LOWEST, tokens)

    val expected = Buildin(
      BuildinFn.Logical(
        BuildinFn.Or,
        LiteralExpr(true),
        Buildin(
          BuildinFn.Logical(
            BuildinFn.And,
            LiteralExpr(false),
            LiteralExpr(true)
          )
        )
      )
    )

    assertEquals(parserOut, Right(ParserOut(expected, List())))
  }

  // test for multiple AND operator
  test(s"parse_primary_boolean_and_and") {
    val tokens = List(True, And, False, And, True)
    val parserOut = JSParser().parsePrecedence(Precendence.LOWEST, tokens)

    val expected = Buildin(
      BuildinFn.Logical(
        BuildinFn.And,
        Buildin(
          BuildinFn.Logical(
            BuildinFn.And,
            LiteralExpr(true),
            LiteralExpr(false)
          )
        ),
        LiteralExpr(true)
      )
    )

    assertEquals(parserOut, Right(ParserOut(expected, List())))
  }

  // test 1 + 2 * 3 - 6 / 7
  test(s"parse_primary_arithmetic") {
    val tokens = List(
      Number("1"),
      Plus,
      Number("2"),
      Star,
      Number("3"),
      Minus,
      Number("6"),
      Slash,
      Number("7")
    )
    val parserOut = JSParser().parsePrecedence(Precendence.LOWEST, tokens)

    val expected = Buildin(
      BuildinFn.Arithmetic(
        BuildinFn.Sub,
        Buildin(
          BuildinFn.Arithmetic(
            BuildinFn.Add,
            LiteralExpr(1),
            Buildin(
              BuildinFn.Arithmetic(
                BuildinFn.Mul,
                LiteralExpr(2),
                LiteralExpr(3)
              )
            )
          )
        ),
        Buildin(
          BuildinFn.Arithmetic(
            BuildinFn.Div,
            LiteralExpr(6),
            LiteralExpr(7)
          )
        )
      )
    )

    assertEquals(parserOut, Right(ParserOut(expected, List())))
  }

  // test 1 > 2 OR 3 == 4 AND 5 < 6
  test(s"parse_primary_complex_boolean") {
    val tokens = List(
      Number("1"),
      Greater,
      Number("2"),
      Or,
      Number("3"),
      EqualEqual,
      Number("4"),
      And,
      Number("5"),
      Less,
      Number("6")
    )
    val parserOut = JSParser().expression(tokens)

    val expected = Buildin(
      BuildinFn.Logical(
        BuildinFn.Or,
        Buildin(
          BuildinFn.Comparison(
            BuildinFn.Greater,
            LiteralExpr(1),
            LiteralExpr(2)
          )
        ),
        Buildin(
          BuildinFn.Logical(
            BuildinFn.And,
            Buildin(
              BuildinFn.Comparison(
                BuildinFn.Equal,
                LiteralExpr(3),
                LiteralExpr(4)
              )
            ),
            Buildin(
              BuildinFn.Comparison(
                BuildinFn.Less,
                LiteralExpr(5),
                LiteralExpr(6)
              )
            )
          )
        )
      )
    )

    assertEquals(parserOut, Right(ParserOut(expected, List())))

  }

  // test (1 + 2) * 3 > 4 OR 5 < (6 + 7)
  test(s"parse_primary_complex_boolean_2") {
    val tokens = List(
      LeftParen,
      Number("1"),
      Plus,
      Number("2"),
      RightParen,
      Star,
      Number("3"),
      Greater,
      Number("4"),
      Or,
      Number("5"),
      Less,
      LeftParen,
      Number("6"),
      Plus,
      Number("7"),
      RightParen
    )
    val parserOut = JSParser().expression(tokens)

    val expected = Buildin(
      BuildinFn.Logical(
        BuildinFn.Or,
        Buildin(
          BuildinFn.Comparison(
            BuildinFn.Greater,
            Buildin(
              BuildinFn.Arithmetic(
                BuildinFn.Mul,
                Buildin(
                  BuildinFn.Arithmetic(
                    BuildinFn.Add,
                    LiteralExpr(1),
                    LiteralExpr(2)
                  )
                ),
                LiteralExpr(3)
              )
            ),
            LiteralExpr(4)
          )
        ),
        Buildin(
          BuildinFn.Comparison(
            BuildinFn.Less,
            LiteralExpr(5),
            Buildin(
              BuildinFn.Arithmetic(
                BuildinFn.Add,
                LiteralExpr(6),
                LiteralExpr(7)
              )
            )
          )
        )
      )
    )

    assertEquals(parserOut, Right(ParserOut(expected, List())))
  }

  // test a + b > 5
  test(s"parse_primary_complex_boolean_3") {
    val tokens = List(
      Identifier("a"),
      Plus,
      Identifier("b"),
      Greater,
      Number("5")
    )
    val parserOut = JSParser().expression(tokens)

    val expected = Buildin(
      BuildinFn.Comparison(
        BuildinFn.Greater,
        Buildin(
          BuildinFn.Arithmetic(
            BuildinFn.Add,
            Variable(tokenA),
            Variable(tokenB)
          )
        ),
        LiteralExpr(5)
      )
    )

    assertEquals(parserOut, Right(ParserOut(expected, List())))
  }

  // test unary: -a + b
  test(s"parse_primary_unary") {
    val tokens = List(
      Minus,
      Identifier("a"),
      Plus,
      Identifier("b")
    )
    val parserOut = JSParser().expression(tokens)

    val expected = Buildin(
      BuildinFn.Arithmetic(
        BuildinFn.Add,
        Buildin(
          BuildinFn.Unary(
            BuildinFn.Negate,
            Variable(tokenA)
          )
        ),
        Variable(tokenB)
      )
    )

    assertEquals(parserOut, Right(ParserOut(expected, List())))
  }

  // test NOT and OR operator
  test(s"parse_primary_boolean_not_or") {
    val tokens = List(Bang, True, Or, False)
    val parserOut = JSParser().expression(tokens)

    val expected = Buildin(
      BuildinFn.Logical(
        BuildinFn.Or,
        Buildin(
          BuildinFn.Unary(
            BuildinFn.Not,
            LiteralExpr(true)
          )
        ),
        LiteralExpr(false)
      )
    )

    assertEquals(parserOut, Right(ParserOut(expected, List())))
  }

  // test NOT and parenthesis and OR operator (NOT (true OR false))
  test(s"parse_primary_boolean_not_or_parenthesis") {
    val tokens = List(Bang, LeftParen, True, Or, False, RightParen)
    val parserOut = JSParser().expression(tokens)

    val expected = Buildin(
      BuildinFn.Unary(
        BuildinFn.Not,
        Buildin(
          BuildinFn.Logical(
            BuildinFn.Or,
            LiteralExpr(true),
            LiteralExpr(false)
          )
        )
      )
    )

    assertEquals(parserOut, Right(ParserOut(expected, List())))
  }

  // test conditional operator
  test(s"parse_primary_conditional") {
    val tokens = List(
      Keyword.If,
      Identifier("a"),
      Keyword.Then,
      Identifier("b"),
      Keyword.Else,
      Identifier("c")
    )
    val parserOut = JSParser().expression(tokens)

    val expected = Cond(
      Variable(tokenA),
      Variable(tokenB),
      Variable(tokenC)
    )

    assertEquals(parserOut, Right(ParserOut(expected, List())))
  }

  // test: if a == b then 2 else 4 + 8
  test(s"parse_primary_conditional_2") {
    val tokens = List(
      Keyword.If,
      Identifier("a"),
      EqualEqual,
      Identifier("b"),
      Keyword.Then,
      Number("2"),
      Keyword.Else,
      Number("4"),
      Plus,
      Number("8")
    )
    val parserOut = JSParser().expression(tokens)

    val expected = Cond(
      Buildin(
        BuildinFn.Comparison(
          BuildinFn.Equal,
          Variable(tokenA),
          Variable(tokenB)
        )
      ),
      LiteralExpr(2),
      Buildin(
        BuildinFn.Arithmetic(
          BuildinFn.Add,
          LiteralExpr(4),
          LiteralExpr(8)
        )
      )
    )

    assertEquals(parserOut, Right(ParserOut(expected, List())))
  }

  // test: 5 + if a == b then 6 else 8
  test(s"parse_primary_conditional_3") {
    val tokens = List(
      Number("5"),
      Plus,
      Keyword.If,
      Identifier("a"),
      EqualEqual,
      Identifier("b"),
      Keyword.Then,
      Number("6"),
      Keyword.Else,
      Number("8")
    )
    val parserOut = JSParser().expression(tokens)

    val expected = Buildin(
      BuildinFn.Arithmetic(
        BuildinFn.Add,
        LiteralExpr(5),
        Cond(
          Buildin(
            BuildinFn.Comparison(
              BuildinFn.Equal,
              Variable(tokenA),
              Variable(tokenB)
            )
          ),
          LiteralExpr(6),
          LiteralExpr(8)
        )
      )
    )

    assertEquals(parserOut, Right(ParserOut(expected, List())))
  }

  // test app with multiple args: f(a, b)
  test(s"parse_primary_app") {
    val tokens = List(
      Identifier("f"),
      LeftParen,
      Identifier("a"),
      Comma,
      Identifier("b"),
      RightParen
    )
    val parserOut = JSParser().expression(tokens)

    val expected = App(
      App(body = Variable(tokenF), arg = Variable(tokenA)),
      arg = Variable(tokenB)
    )

    assertEquals(parserOut, Right(ParserOut(expected, List())))
  }

  // test app with multiple args: f(a, b, c)
  test(s"parse_primary_app_2") {
    val tokens = List(
      Identifier("f"),
      LeftParen,
      Identifier("a"),
      Comma,
      Identifier("b"),
      Comma,
      Identifier("c"),
      RightParen
    )
    val parserOut = JSParser().expression(tokens)

    val expected = App(
      App(
        App(body = Variable(tokenF), arg = Variable(tokenA)),
        arg = Variable(tokenB)
      ),
      arg = Variable(tokenC)
    )

    assertEquals(parserOut, Right(ParserOut(expected, List())))
  }

  /*
  Test Binding operator
  Let a = 1
  in a + 1
   */
  test(s"parse_binding") {
    val tokens = List(
      Keyword.Let,
      Identifier("a"),
      Equal,
      Number("1"),
      Keyword.In,
      Identifier("a"),
      Plus,
      Number("1")
    )
    val parserOut = JSParser().expression(tokens)

    val expected = Binding(
      recursive = false,
      Variable(tokenA),
      LiteralExpr(1),
      Buildin(
        BuildinFn.Arithmetic(
          BuildinFn.Add,
          Variable(tokenA),
          LiteralExpr(1)
        )
      )
    )

    assertEquals(parserOut, Right(ParserOut(expected, List())))
  }

  /*
  Test Lambda function and Abs Operator
  fun x -> x + 1
   */
  test(s"parse_lambda") {
    val tokens = List(
      Keyword.Fun,
      Identifier("x"),
      Arrow,
      Identifier("x"),
      Plus,
      Number("1")
    )
    val parserOut = JSParser().expression(tokens)

    val expected = Abs(
      Variable(tokenX),
      variableType = TAny,
      body = Buildin(
        BuildinFn.Arithmetic(
          BuildinFn.Add,
          Variable(tokenX),
          LiteralExpr(1)
        )
      )
    )

    assertEquals(parserOut, Right(ParserOut(expected, List())))
  }

  /* Test Lambda function and Abs Operator
  fun x y -> x + y
   */
  test(s"parse_lambda_2") {
    val tokens = List(
      Keyword.Fun,
      Identifier("x"),
      Identifier("y"),
      Arrow,
      Identifier("x"),
      Plus,
      Identifier("y")
    )
    val parserOut = JSParser().expression(tokens)

    val expected = Abs(
      Variable(tokenX),
      variableType = TAny,
      body = Abs(
        Variable(tokenY),
        variableType = TAny,
        body = Buildin(
          BuildinFn.Arithmetic(
            BuildinFn.Add,
            Variable(tokenX),
            Variable(tokenY)
          )
        )
      )
    )

    assertEquals(parserOut, Right(ParserOut(expected, List())))
  }

  /* Test Lambda function and Abs Operator in Let
    Let a = 1
    Let b = 2
    Let sum = fun x y -> x + y
    In sum (a , b)
   */
  test(s"parse_lambda_3") {
    val tokens = List(
      Keyword.Let,
      Identifier("a"),
      Equal,
      Number("1"),
      Keyword.Let,
      Identifier("b"),
      Equal,
      Number("2"),
      Keyword.Let,
      Identifier("sum"),
      Equal,
      Keyword.Fun,
      Identifier("x"),
      Identifier("y"),
      Arrow,
      Identifier("x"),
      Plus,
      Identifier("y"),
      Keyword.In,
      Identifier("sum"),
      LeftParen,
      Identifier("a"),
      Comma,
      Identifier("b"),
      RightParen
    )
    val parserOut = JSParser().expression(tokens)

    val expected = Binding(
      recursive = false,
      Variable(tokenA),
      LiteralExpr(1),
      Binding(
        recursive = false,
        Variable(tokenB),
        LiteralExpr(2),
        Binding(
          recursive = false,
          Variable(tokenSum),
          Abs(
            Variable(tokenX),
            variableType = TAny,
            body = Abs(
              Variable(tokenY),
              variableType = TAny,
              body = Buildin(
                BuildinFn.Arithmetic(
                  BuildinFn.Add,
                  Variable(tokenX),
                  Variable(tokenY)
                )
              )
            )
          ),
          App(
            App(
              body = Variable(tokenSum),
              arg = Variable(tokenA)
            ),
            arg = Variable(tokenB)
          )
        )
      )
    )

    assertEquals(parserOut, Right(ParserOut(expected, List())))
  }

  /*
  Test recursive function
  let rec fact = fun x -> if x == 0 then 1 else x * fact(x - 1)
  in fact(5)
   */
  test(s"parse_recursive_function") {
    val tokens = List(
      Keyword.Let,
      Keyword.Rec,
      Identifier("fact"),
      Equal,
      Keyword.Fun,
      Identifier("x"),
      Arrow,
      Keyword.If,
      Identifier("x"),
      EqualEqual,
      Number("0"),
      Keyword.Then,
      Number("1"),
      Keyword.Else,
      Identifier("x"),
      Star,
      Identifier("fact"),
      LeftParen,
      Identifier("x"),
      Minus,
      Number("1"),
      RightParen,
      Keyword.In,
      Identifier("fact"),
      LeftParen,
      Number("5"),
      RightParen
    )
    val parserOut = JSParser().expression(tokens)

    val expected = Binding(
      recursive = true,
      Variable(tokenFact),
      Abs(
        Variable(tokenX),
        variableType = TAny,
        body = Cond(
          Buildin(
            BuildinFn.Comparison(
              BuildinFn.Equal,
              Variable(tokenX),
              LiteralExpr(0)
            )
          ),
          LiteralExpr(1),
          Buildin(
            BuildinFn.Arithmetic(
              BuildinFn.Mul,
              Variable(tokenX),
              App(
                body = Variable(tokenFact),
                arg = Buildin(
                  BuildinFn.Arithmetic(
                    BuildinFn.Sub,
                    Variable(tokenX),
                    LiteralExpr(1)
                  )
                )
              )
            )
          )
        )
      ),
      App(
        body = Variable(tokenFact),
        arg = LiteralExpr(5)
      )
    )

    assertEquals(parserOut, Right(ParserOut(expected, List())))
  }

  // Test tuple expression
  // Let a = (1, 2)
  // in a
  test(s"parse_tuple") {
    val tokens = List(
      Keyword.Let,
      Identifier("a"),
      Equal,
      LeftParen,
      Number("1"),
      Comma,
      Number("2"),
      RightParen,
      Keyword.In,
      Identifier("a")
    )
    val parserOut = JSParser().expression(tokens)

    val expected = Binding(
      recursive = false,
      Variable(tokenA),
      TupleExpr(LiteralExpr(1), LiteralExpr(2)),
      Variable(tokenA)
    )

    assertEquals(parserOut, Right(ParserOut(expected, List())))
  }

  // test nested tuple expression
  // Let a = (1, 2, 3)
  // in a
  test(s"parse_nested_tuple") {
    val tokens = List(
      Keyword.Let,
      Identifier("a"),
      Equal,
      LeftParen,
      Number("1"),
      Comma,
      Number("2"),
      Comma,
      Number("3"),
      RightParen,
      Keyword.In,
      Identifier("a")
    )
    val parserOut = JSParser().expression(tokens)

    val expected = Binding(
      recursive = false,
      Variable(tokenA),
      TupleExpr(
        TupleExpr(
          LiteralExpr(1),
          LiteralExpr(2)
        ),
        LiteralExpr(3)
      ),
      Variable(tokenA)
    )

    assertEquals(parserOut, Right(ParserOut(expected, List())))
  }

  // test condition: if (4 > 5) and true then 1 else 2
  test(s"parse_condition") {
    val tokens = List(
      Keyword.If,
      LeftParen,
      Number("4"),
      Greater,
      Number("5"),
      RightParen,
      Keyword.And,
      Keyword.True,
      Keyword.Then,
      Number("1"),
      Keyword.Else,
      Number("2")
    )
    val parserOut = JSParser().expression(tokens)

    val expected = Cond(
      Buildin(
        Logical(
          BuildinFn.And,
          Buildin(
            BuildinFn.Comparison(
              BuildinFn.Greater,
              LiteralExpr(4),
              LiteralExpr(5)
            )
          ),
          LiteralExpr(true)
        )
      ),
      LiteralExpr(1),
      LiteralExpr(2)
    )

    assertEquals(parserOut, Right(ParserOut(expected, List())))
  }
