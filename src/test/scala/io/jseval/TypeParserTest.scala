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
import io.jseval.parser.TypeParser
import io.jseval.parser.TypeParserResult

class TypeParserTest extends munit.FunSuite:

  // test primary type
  // string
  test("parse_string_type") {
    val tokens = List(StringKw)
    val result = TypeParser.parseType(tokens)
    assertEquals(result, Right(TypeParserResult(TString, List())))
  }

  // test primary type
  // Int
  test("parse_int_type") {
    val tokens = List(IntKw)
    val result = TypeParser.parseType(tokens)
    assertEquals(result, Right(TypeParserResult(TInt, List())))
  }

  // test primary type
  // Boolean
  test("parse_boolean_type") {
    val tokens = List(BooleanKw)
    val result = TypeParser.parseType(tokens)
    assertEquals(result, Right(TypeParserResult(TBoolean, List())))
  }

  // test primary type
  // Unit
  test("parse_unit_type") {
    val tokens = List(Unit)
    val result = TypeParser.parseType(tokens)
    assertEquals(result, Right(TypeParserResult(TUnit, List())))
  }

  // test arrow type
  // Int -> Int -> Int
  test("parse_arrow_type") {
    val tokens = List(IntKw, ArrowToken, IntKw, ArrowToken, IntKw)
    val result = TypeParser.parseType(tokens)
    assertEquals(
      result,
      Right(
        TypeParserResult(
          TArrow(TArrow(TInt, TInt), TInt),
          List()
        )
      )
    )
  }

  // test product type
  // Int * Int * Int
  test("parse_product_type") {
    val tokens = List(IntKw, StarToken, IntKw, StarToken, IntKw)
    val result = TypeParser.parseType(tokens)
    assertEquals(
      result,
      Right(
        TypeParserResult(
          TProduct(TProduct(TInt, TInt), TInt),
          List()
        )
      )
    )
  }

  // test nested product and arrow type
  // Int => Boolean * Boolean => String
  test("parse_nested_product_and_arrow_type") {
    val tokens = List(
      IntKw,
      ArrowToken,
      BooleanKw,
      StarToken,
      BooleanKw,
      ArrowToken,
      StringKw
    )
    val result = TypeParser.parseType(tokens)
    assertEquals(
      result,
      Right(
        TypeParserResult(
          TArrow(
            TArrow(
              TInt,
              TProduct(TBoolean, TBoolean)
            ),
            TString
          ),
          List()
        )
      )
    )
  }
