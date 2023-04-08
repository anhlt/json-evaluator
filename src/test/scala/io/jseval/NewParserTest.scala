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

class NewParserTest extends munit.FunSuite:

  val tokenA = Literal.Identifier("a")
  val tokenB = Literal.Identifier("b")
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
                Grouping(
                  Buildin(
                    BuildinFn.Arithmetic(
                      BuildinFn.Add,
                      LiteralExpr(1),
                      LiteralExpr(2)
                    )
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
            Grouping(
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

  // test NOT and parenthesis and OR operator
  test(s"parse_primary_boolean_not_or_2") {
    val tokens = List(Bang, LeftParen, True, Or, False, RightParen)
    val parserOut = JSParser().expression(tokens)
    
  }
