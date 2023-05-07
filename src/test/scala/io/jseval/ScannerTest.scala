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

  test("float_number") {
    val str = "123.456"
    val expected : Either[Scanner.Error, List[Token]] = List(
      FloatNumber(str)
    ).asRight

    assertEquals(Scanner.parse(str), expected)
  }

  // test int number
  test("int_number") {
    val str = "123"
    val expected : Either[Scanner.Error, List[Token]] = List(
      Number(str)
    ).asRight
  }


  test("Scanner: List") {
    val str =
      """
        |[1, 2, 3, 4]
        |""".stripMargin

    val expected: Either[Scanner.Error, List[Token]] = (
      List(
        Operator.LeftBracketToken,
        Literal.Number("1"),
        CommaToken,
        Literal.Number("2"),
        CommaToken,
        Literal.Number("3"),
        CommaToken,
        Literal.Number("4"),
        Operator.RightBracketToken
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
        GreaterToken,
        Number(
          "3"
        ),
        OrKw,
        LeftParenToken,
        Number(
          "5"
        ),
        PlusToken,
        Number(
          "6"
        ),
        LessToken,
        Number(
          "4"
        ),
        RightParenToken,
        AndKw,
        LeftParenToken,
        LeftParenToken,
        Number(
          "7"
        ),
        PlusToken,
        Number(
          "8"
        ),
        RightParenToken,
        GreaterToken,
        Number(
          "3"
        ),
        RightParenToken
      )
      ).asRight

    assertEquals(Scanner.parse(str), expected)

  }

}
