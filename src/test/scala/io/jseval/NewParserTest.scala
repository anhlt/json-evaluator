package io.jseval

import cats.*
import cats.implicits.*
import Expression.*
import Expression.BuildinModule.BuildinFn
import Expression.ValueModule.*
import Keyword.*
import Operator.*
import Literal.*
import io.jseval.parser.{ExpressionParser, ParserOut, Precendence}
import io.jseval.TypModule.*

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

    val tokens = List(FloatNumber("1.0"))
    val parserOut = ExpressionParser.parsePrecedence(Precendence.LOWEST, tokens)
    assertEquals(parserOut, Right(ParserOut(LiteralExpr(1.0), List())))
  }

  test(s"parse_primary_string") {
    val tokens = List(Str("hello"))
    val parserOut = ExpressionParser.parsePrecedence(Precendence.LOWEST, tokens)
    assertEquals(parserOut, Right(ParserOut(LiteralExpr("hello"), List())))
  }

  test(s"parse_primary_identifier") {
    val tokens = List(tokenX)
    val parserOut = ExpressionParser.parsePrecedence(Precendence.LOWEST, tokens)
    assertEquals(parserOut, Right(ParserOut(Variable(tokenX), List())))
  }

  test(s"parse_primary_boolean") {
    val tokens = List(TrueKw)
    val parserOut = ExpressionParser.parsePrecedence(Precendence.LOWEST, tokens)
    assertEquals(parserOut, Right(ParserOut(LiteralExpr(true), List())))
  }

  //   test for parsing AND OR operator for JSParser
  test(s"parse_primary_boolean_and") {
    val tokens = List(TrueKw, AndKw, FalseKw)
    val parserOut = ExpressionParser.parsePrecedence(Precendence.LOWEST, tokens)

    val expected = Buildin(
      BuildinFn.Logical(
        BuildinFn.And,
        LiteralExpr(true),
        LiteralExpr(false)
      )
    )

    assertEquals(parserOut, Right(ParserOut(expected, List())))
  }

  //   test combination of And andOrKw precedence
  test(s"parse_primary_boolean_and_or") {
    val tokens = List(TrueKw, AndKw, FalseKw, OrKw, TrueKw)
    val parserOut = ExpressionParser.parsePrecedence(Precendence.LOWEST, tokens)

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

  // test combinataion ofOrKw and And precedence
  test(s"parse_primary_boolean_or_and") {
    val tokens = List(TrueKw, OrKw, FalseKw, AndKw, TrueKw)
    val parserOut = ExpressionParser.parsePrecedence(Precendence.LOWEST, tokens)

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
    val tokens = List(TrueKw, AndKw, FalseKw, AndKw, TrueKw)
    val parserOut = ExpressionParser.parsePrecedence(Precendence.LOWEST, tokens)

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
      PlusToken,
      Number("2"),
      StarToken,
      Number("3"),
      MinusToken,
      Number("6"),
      SlashToken,
      Number("7")
    )
    val parserOut = ExpressionParser.parsePrecedence(Precendence.LOWEST, tokens)

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
      GreaterToken,
      Number("2"),
      OrKw,
      Number("3"),
      EqualEqualToken,
      Number("4"),
      AndKw,
      Number("5"),
      LessToken,
      Number("6")
    )
    val parserOut = ExpressionParser.expression(tokens)

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
      LeftParenToken,
      Number("1"),
      PlusToken,
      Number("2"),
      RightParenToken,
      StarToken,
      Number("3"),
      GreaterToken,
      Number("4"),
      OrKw,
      Number("5"),
      LessToken,
      LeftParenToken,
      Number("6"),
      PlusToken,
      Number("7"),
      RightParenToken
    )
    val parserOut = ExpressionParser.expression(tokens)

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
      PlusToken,
      Identifier("b"),
      GreaterToken,
      Number("5")
    )
    val parserOut = ExpressionParser.expression(tokens)

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
      MinusToken,
      Identifier("a"),
      PlusToken,
      Identifier("b")
    )
    val parserOut = ExpressionParser.expression(tokens)

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
    val tokens = List(BangToken, TrueKw, OrKw, FalseKw)
    val parserOut = ExpressionParser.expression(tokens)

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
    val tokens =
      List(BangToken, LeftParenToken, TrueKw, OrKw, FalseKw, RightParenToken)
    val parserOut = ExpressionParser.expression(tokens)

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
      Keyword.IfKw,
      Identifier("a"),
      Keyword.ThenKw,
      Identifier("b"),
      Keyword.ElseKw,
      Identifier("c")
    )
    val parserOut = ExpressionParser.expression(tokens)

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
      Keyword.IfKw,
      Identifier("a"),
      EqualEqualToken,
      Identifier("b"),
      Keyword.ThenKw,
      Number("2"),
      Keyword.ElseKw,
      Number("4"),
      PlusToken,
      Number("8")
    )
    val parserOut = ExpressionParser.expression(tokens)

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
      PlusToken,
      Keyword.IfKw,
      Identifier("a"),
      EqualEqualToken,
      Identifier("b"),
      Keyword.ThenKw,
      Number("6"),
      Keyword.ElseKw,
      Number("8")
    )
    val parserOut = ExpressionParser.expression(tokens)

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
      LeftParenToken,
      Identifier("a"),
      CommaToken,
      Identifier("b"),
      RightParenToken
    )
    val parserOut = ExpressionParser.expression(tokens)

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
      LeftParenToken,
      Identifier("a"),
      CommaToken,
      Identifier("b"),
      CommaToken,
      Identifier("c"),
      RightParenToken
    )
    val parserOut = ExpressionParser.expression(tokens)

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
      Keyword.LetKw,
      Identifier("a"),
      EqualToken,
      Number("1"),
      Keyword.InKw,
      Identifier("a"),
      PlusToken,
      Number("1")
    )
    val parserOut = ExpressionParser.expression(tokens)

    val expected = Binding(
      recursive = false,
      Variable(tokenA),
      variableType = None,
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
      Keyword.FunKw,
      Identifier("x"),
      ArrowToken,
      Identifier("x"),
      PlusToken,
      Number("1")
    )
    val parserOut = ExpressionParser.expression(tokens)

    val expected = Abs(
      Variable(tokenX),
      variableType = None,
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
      Keyword.FunKw,
      Identifier("x"),
      Identifier("y"),
      ArrowToken,
      Identifier("x"),
      PlusToken,
      Identifier("y")
    )
    val parserOut = ExpressionParser.expression(tokens)

    val expected = Abs(
      Variable(tokenX),
      variableType = None,
      body = Abs(
        Variable(tokenY),
        variableType = None,
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
      Keyword.LetKw,
      Identifier("a"),
      EqualToken,
      Number("1"),
      Keyword.LetKw,
      Identifier("b"),
      EqualToken,
      Number("2"),
      Keyword.LetKw,
      Identifier("sum"),
      EqualToken,
      Keyword.FunKw,
      Identifier("x"),
      Identifier("y"),
      ArrowToken,
      Identifier("x"),
      PlusToken,
      Identifier("y"),
      Keyword.InKw,
      Identifier("sum"),
      LeftParenToken,
      Identifier("a"),
      CommaToken,
      Identifier("b"),
      RightParenToken
    )
    val parserOut = ExpressionParser.expression(tokens)

    val expected = Binding(
      recursive = false,
      Variable(tokenA),
      variableType = None,
      LiteralExpr(1),
      Binding(
        recursive = false,
        Variable(tokenB),
        variableType = None,
        LiteralExpr(2),
        Binding(
          recursive = false,
          Variable(tokenSum),
          variableType = None,
          Abs(
            Variable(tokenX),
            variableType = None,
            body = Abs(
              Variable(tokenY),
              variableType = None,
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
      Keyword.LetKw,
      Keyword.RecKw,
      Identifier("fact"),
      EqualToken,
      Keyword.FunKw,
      Identifier("x"),
      ArrowToken,
      Keyword.IfKw,
      Identifier("x"),
      EqualEqualToken,
      Number("0"),
      Keyword.ThenKw,
      Number("1"),
      Keyword.ElseKw,
      Identifier("x"),
      StarToken,
      Identifier("fact"),
      LeftParenToken,
      Identifier("x"),
      MinusToken,
      Number("1"),
      RightParenToken,
      Keyword.InKw,
      Identifier("fact"),
      LeftParenToken,
      Number("5"),
      RightParenToken
    )
    val parserOut = ExpressionParser.expression(tokens)

    val expected = Binding(
      recursive = true,
      Variable(tokenFact),
      variableType = None,
      Abs(
        Variable(tokenX),
        variableType = None,
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
      Keyword.LetKw,
      Identifier("a"),
      EqualToken,
      LeftParenToken,
      Number("1"),
      CommaToken,
      Number("2"),
      RightParenToken,
      Keyword.InKw,
      Identifier("a")
    )
    val parserOut = ExpressionParser.expression(tokens)

    val expected = Binding(
      recursive = false,
      Variable(tokenA),
      variableType = None,
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
      Keyword.LetKw,
      Identifier("a"),
      EqualToken,
      LeftParenToken,
      Number("1"),
      CommaToken,
      Number("2"),
      CommaToken,
      Number("3"),
      RightParenToken,
      Keyword.InKw,
      Identifier("a")
    )
    val parserOut = ExpressionParser.expression(tokens)

    val expected = Binding(
      recursive = false,
      Variable(tokenA),
      variableType = None,
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
      Keyword.IfKw,
      LeftParenToken,
      Number("4"),
      GreaterToken,
      Number("5"),
      RightParenToken,
      Keyword.AndKw,
      Keyword.TrueKw,
      Keyword.ThenKw,
      Number("1"),
      Keyword.ElseKw,
      Number("2")
    )
    val parserOut = ExpressionParser.expression(tokens)

    val expected = Cond(
      Buildin(
        BuildinFn.Logical(
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

  // test: f (1, 2, g(1, 2))
  test(s"parse_app_with_multiple_args") {
    val tokens = List(
      Identifier("f"),
      LeftParenToken,
      Number("1"),
      CommaToken,
      Number("2"),
      CommaToken,
      Identifier("g"),
      LeftParenToken,
      Number("1"),
      CommaToken,
      Number("2"),
      RightParenToken,
      RightParenToken
    )
    val parserOut = ExpressionParser.expression(tokens)

    val expected = App(
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

    assertEquals(parserOut, Right(ParserOut(expected, List())))
  }

  // test
  // Let sum = fun (x: int) (y: int) -> x + y
  // In sum (a , b)
  test(s"parse_app_with_multiple_args_2") {
    val tokens = List(
      Keyword.LetKw,
      Identifier("sum"),
      EqualToken,
      Keyword.FunKw,
      LeftParenToken,
      Identifier("x"),
      ColonToken,
      IntKw,
      RightParenToken,
      LeftParenToken,
      Identifier("y"),
      ColonToken,
      IntKw,
      RightParenToken,
      ArrowToken,
      Identifier("x"),
      PlusToken,
      Identifier("y"),
      Keyword.InKw,
      Identifier("sum"),
      LeftParenToken,
      Identifier("a"),
      CommaToken,
      Identifier("b"),
      RightParenToken
    )
    val parserOut = ExpressionParser.expression(tokens)

    val expected = Binding(
      recursive = false,
      Variable(tokenSum),
      variableType = None,
      Abs(
        Variable(tokenX),
        Some(TInt),
        Abs(
          Variable(tokenY),
          Some(TInt),
          Buildin(
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
          Variable(tokenSum),
          Variable(tokenA)
        ),
        Variable(tokenB)
      )
    )

    assertEquals(parserOut, Right(ParserOut(expected, List())))
  }

  // test let parser
  // |let z : int = 4
  // |let u : string = 3
  // |let sum = fun x y -> x + y
  // |in sum(z, u)
  test(s"parse_let") {
    val tokens = List(
      Keyword.LetKw,
      Identifier("z"),
      ColonToken,
      IntKw,
      EqualToken,
      Number("4"),
      Keyword.LetKw,
      Identifier("u"),
      ColonToken,
      StringKw,
      EqualToken,
      Number("3"),
      Keyword.LetKw,
      Identifier("sum"),
      EqualToken,
      Keyword.FunKw,
      Identifier("x"),
      Identifier("y"),
      ArrowToken,
      Identifier("x"),
      PlusToken,
      Identifier("y"),
      Keyword.InKw,
      Identifier("sum"),
      LeftParenToken,
      Identifier("z"),
      CommaToken,
      Identifier("u"),
      RightParenToken
    )
    val parserOut = ExpressionParser.expression(tokens)

    val expected = Binding(
      recursive = false,
      Variable(tokenZ),
      variableType = Some(TInt),
      LiteralExpr(4),
      Binding(
        recursive = false,
        Variable(tokenU),
        variableType = Some(TString),
        LiteralExpr(3),
        Binding(
          recursive = false,
          Variable(tokenSum),
          variableType = None,
          Abs(
            Variable(tokenX),
            None,
            Abs(
              Variable(tokenY),
              None,
              Buildin(
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
              Variable(tokenSum),
              Variable(tokenZ)
            ),
            Variable(tokenU)
          )
        )
      )
    )

    assertEquals(parserOut, Right(ParserOut(expected, List())))
  }

  //test bind with type
  //let sum = fun (x: int) (y : int) -> x + y  
  //in sum(1, 2)
  test(s"parse_let_with_type") {
    val tokens = List(
      Keyword.LetKw,
      Identifier("sum"),
      EqualToken,
      Keyword.FunKw,
      LeftParenToken,
      Identifier("x"),
      ColonToken,
      IntKw,
      RightParenToken,
      LeftParenToken,
      Identifier("y"),
      ColonToken,
      IntKw,
      RightParenToken,
      ArrowToken,
      Identifier("x"),
      PlusToken,
      Identifier("y"),
      Keyword.InKw,
      Identifier("sum"),
      LeftParenToken,
      Number("1"),
      CommaToken,
      Number("2"),
      RightParenToken
    )
    val parserOut = ExpressionParser.expression(tokens)

    val expected = Binding(
      recursive = false,
      Variable(tokenSum),
      variableType = None,
      Abs(
        Variable(tokenX),
        Some(TInt),
        Abs(
          Variable(tokenY),
          Some(TInt),
          Buildin(
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
          Variable(tokenSum),
          LiteralExpr(1)
        ),
        LiteralExpr(2)
      )
    )

    assertEquals(parserOut, Right(ParserOut(expected, List())))
  }
