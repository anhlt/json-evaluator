package io.jseval

import Parser._
import cats.{parse => _, _}
import cats.implicits._
import munit.CatsEffectSuite
import Expression as Expr
import io.jseval.Scanner.{parser => ScannerParser}

import Keyword._
import Operator._
import Literal._
import io.jseval.Expression.Expr

class ParserTest extends munit.FunSuite:

  test(s"expression primary number") {
    val ts = List(Literal.Number("42"))
    val expected = Expression.Literal(42)

    assertEquals(parse(ts), Right(expected, Nil))
  }

  test("expression primary string") {
    val ts = List(Literal.Str("you rox!"))
    val want = Expr.Literal("you rox!")
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

    val want = Expr.Or(
      left = Expr.Greater(
        left = Expr.Literal(
          rawValue = 4.0
        ),
        right = Expr.Literal(
          rawValue = 3.0
        )
      ),
      right = Expr.And(
        left = Expr.Grouping(
          expr = Expr.Less(
            left = Expr.Add(
              left = Expr.Literal(
                rawValue = 5.0
              ),
              right = Expr.Literal(
                rawValue = 6.0
              )
            ),
            right = Expr.Literal(
              rawValue = 4.0
            )
          )
        ),
        right = Expr.Grouping(
          expr = Expr.Greater(
            left = Expr.Grouping(
              expr = Expr.Add(
                left = Expr.Literal(
                  rawValue = 7.0
                ),
                right = Expr.Literal(
                  rawValue = 8.0
                )
              )
            ),
            right = Expr.Literal(
              rawValue = 3.0
            )
          )
        )
      )
    )

    assertEquals(parse(ts), Right(want, Nil))

  }
