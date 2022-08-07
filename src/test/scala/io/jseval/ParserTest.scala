package io.jseval

import Parser._
import cats.{parse => _, _}
import cats.implicits._
import munit.CatsEffectSuite
import Expression as Expr
import Expression.{Buildin, BuildinFn}
import io.jseval.Scanner.{parser => ScannerParser}

import Keyword._
import Operator._
import Literal._

class ParserTest extends munit.FunSuite:

  test(s"expression primary number") {
    val ts = List(Literal.Number("42"))
    val expected = Expression.LiteralExpr(42)

    assertEquals(parse(ts), Right(expected, Nil))
  }

  test("expression primary string") {
    val ts = List(Literal.Str("you rox!"))
    val want = Expr.LiteralExpr("you rox!")
    assertEquals(parse(ts), Right(want, Nil))

  }

  test("Expression: '4 > 3 OR (5 + 6 < 4) AND ((7 + 8) > 3)'") {
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
            Expr.LiteralExpr(
              4.0
            ),
            Expr.LiteralExpr(
              3.0
            )
          )
        ),
        Buildin(
          BuildinFn.Logical(
            BuildinFn.And,
            Expr.Grouping(
              expr = Buildin(
                BuildinFn.Comparison(
                  BuildinFn.Less,
                  Buildin(
                    BuildinFn.Arthimetric(
                      BuildinFn.Add,
                      Expr.LiteralExpr(
                        5.0
                      ),
                      Expr.LiteralExpr(
                        6.0
                      )
                    )
                  ),
                  Expr.LiteralExpr(
                    4.0
                  )
                )
              )
            ),
            Expr.Grouping(
              expr = Buildin(
                BuildinFn.Comparison(
                  BuildinFn.Greater,
                  Expr.Grouping(
                    expr = Buildin(
                      BuildinFn.Arthimetric(
                        BuildinFn.Add,
                        Expr.LiteralExpr(
                          7.0
                        ),
                        Expr.LiteralExpr(
                          8.0
                        )
                      )
                    )
                  ),
                  Expr.LiteralExpr(
                    3.0
                  )
                )
              )
            )
          )
        )
      )
    )

    assertEquals(parse(ts), Right(want, Nil))

  }
