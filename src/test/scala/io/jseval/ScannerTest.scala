package io.jseval

import scala.deriving.Mirror

import cats._
import cats.implicits._
import munit.CatsEffectSuite
import org.scalacheck.{Arbitrary, Gen}
import Literal._
import Operator._
import Keyword._

class ScannerTest extends munit.FunSuite {

  test("whilespace empty") {
    assertEquals(Scanner.whitespaces.parseAll(""), Right(()))
  }

  test("Scanner: List") {
    val str =
      """
        |[1, 2, 3, 4]
        |""".stripMargin

    val expected: Either[Scanner.Error, List[Token]] = (
      List(
        Operator.LeftBracket,
        Literal.Number("1"),
        Comma,
        Literal.Number("2"),
        Comma,
        Literal.Number("3"),
        Comma,
        Literal.Number("4"),
        Operator.RightBracket
      )
      ).asRight

    assertEquals(Scanner.parse(str), expected)

  }

  test("logic ") {
    val str =
      """
  	4 > 3 or (5 + 6 < 4) and ((7 + 8) > 3)

  	""".stripMargin

    val expected: Either[Scanner.Error, List[Token]] = (
      List(
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
      ).asRight

    assertEquals(Scanner.parse(str), expected)

  }

}
