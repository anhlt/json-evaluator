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

class ParserTest extends munit.FunSuite:

  test(s"expression primary number") {
    val ts = List(Literal.Number("42"))
    val expected = Expression.LiteralExpr(42)

    assertEquals(parseExpr(ts), Right(expected, Nil))
  }

  test("expression primary string") {
    val ts = List(Literal.Str("you rox!"))
    val want = LiteralExpr("you rox!")
    assertEquals(parseExpr(ts), Right(want, Nil))

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
    assertEquals(parseExpr(ts), Right(want, Nil))
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
      BuildinFn.Arthimetric(
        BuildinFn.Mul,
        Buildin(
          BuildinFn.Arthimetric(
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

    assertEquals(parseExpr(ts), Right(want, Nil))

  }

  test("parse_complex_expression") {

    println("4 > 3 OR (5 + 6 < 4) AND ((7 + 8) > 3)")

    val ts = List(
      Literal.Number(
        "4"
      ),
      Greater,
      Number(
        "3"
      ),
      Or,
      LeftParen,
      Number(
        "5"
      ),
      Plus,
      Number(
        "6"
      ),
      Less,
      Number(
        "4"
      ),
      RightParen,
      And,
      LeftParen,
      LeftParen,
      Number(
        "7"
      ),
      Plus,
      Number(
        "8"
      ),
      RightParen,
      Greater,
      Number(
        "3"
      ),
      RightParen
    )

    val want = Buildin(
      BuildinFn.Logical(
        BuildinFn.Or,
        Buildin(
          BuildinFn.Comparison(
            BuildinFn.Greater,
            LiteralExpr(
              4.0
            ),
            LiteralExpr(
              3.0
            )
          )
        ),
        Buildin(
          BuildinFn.Logical(
            BuildinFn.And,
            Grouping(
              expr = Buildin(
                BuildinFn.Comparison(
                  BuildinFn.Less,
                  Buildin(
                    BuildinFn.Arthimetric(
                      BuildinFn.Add,
                      LiteralExpr(
                        5.0
                      ),
                      LiteralExpr(
                        6.0
                      )
                    )
                  ),
                  LiteralExpr(
                    4.0
                  )
                )
              )
            ),
            Grouping(
              expr = Buildin(
                BuildinFn.Comparison(
                  BuildinFn.Greater,
                  Grouping(
                    expr = Buildin(
                      BuildinFn.Arthimetric(
                        BuildinFn.Add,
                        LiteralExpr(
                          7.0
                        ),
                        LiteralExpr(
                          8.0
                        )
                      )
                    )
                  ),
                  LiteralExpr(
                    3.0
                  )
                )
              )
            )
          )
        )
      )
    )

    assertEquals(parseExpr(ts), Right(want, Nil))

  }

  test("parse_abs") {
    println("fun x y -> x + y + 8")
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
      BuildinFn.Arthimetric(
        BuildinFn.Add,
        Buildin(
          BuildinFn.Arthimetric(
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
    assertEquals(lambdaFunc(ts), Right(want, Nil))

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
    assertEquals(app(ts), Right(want, Nil))

  }
