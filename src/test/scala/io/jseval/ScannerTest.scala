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

  test("logic ") {
    val str = """
  	4 > 3 OR (5 + 6 < 4) AND ((7 + 8) > 3)

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

  test("table_declare.sql") {
    val str = """
CREATE TABLE users (
	phoneTypes LIST[STRING] EXTRACT("$.phoneNumbers[:1].type") ,
    userName STRING EXTRACT("$.firstName") DEFAULT "firstName", 
    amount INT EXTRACT("$.amount") DEFAULT 0
    /* comment */
);
    """.stripMargin
    val expected: List[Token] = List(
      Keyword.Create,
      Keyword.Table,
      Literal.Identifier("users"),
      Operator.LeftParen,
      Literal.Identifier("phoneTypes"),
      Keyword.ListToken,
      Operator.LeftBracket,
      Keyword.String,
      Operator.RightBracket,
      Keyword.Extract,
      Operator.LeftParen,
      Literal.Str("$.phoneNumbers[:1].type"),
      Operator.RightParen,
      Operator.Comma,
      Literal.Identifier("userName"),
      Keyword.String,
      Keyword.Extract,
      Operator.LeftParen,
      Literal.Str("$.firstName"),
      Operator.RightParen,
      Keyword.Default,
      Literal.Str("firstName"),
      Operator.Comma,
      Literal.Identifier("amount"),
      Keyword.Int,
      Keyword.Extract,
      Operator.LeftParen,
      Literal.Str("$.amount"),
      Operator.RightParen,
      Keyword.Default,
      Literal.Number("0"),
      Comment.Block("/* comment */"),
      Operator.RightParen,
      Operator.Semicolon
    )

    assertEquals(Scanner.parse(str), Right(expected))
  }

}
