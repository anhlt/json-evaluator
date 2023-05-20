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
          TArrow(TInt, TArrow(TInt, TInt)),
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
          TProduct(TInt, TProduct(TInt, TInt)),
          List()
        )
      )
    )
  }

  // test right associative arrow type
  // Int -> Bool -> Int
  test("parse_right_associative_arrow_type") {
    val tokens = List(IntKw, ArrowToken, BooleanKw, ArrowToken, IntKw)
    val result = TypeParser.parseType(tokens)
    assertEquals(
      result,
      Right(
        TypeParserResult(
          TArrow(TInt, TArrow(TBoolean, TInt)),
          List()
        )
      )
    )
  }


  // test nested product and arrow type
  // Int -> Boolean * Boolean -> String
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
            TInt,
            TArrow(
              TProduct(TBoolean, TBoolean),
              TString
            )
          ),
          List()
        )
      )
    )
  }

  // Test generic type
  // 'a
  test("parse_generic_type") {
    val tokens = List(ApostropeToken, Identifier("a"))
    val result = TypeParser.parseType(tokens)
    assertEquals(
      result,
      Right(
        TypeParserResult(
          TVar(Identifier("a")),
          List()
        )
      )
    )
  }


  
  // Test generic type with arrow type
  // 'a -> 'b
  test("parse_generic_type_with_arrow_type") {
    val tokens = List(ApostropeToken, Identifier("a"), ArrowToken, ApostropeToken, Identifier("b"))
    val result = TypeParser.parseType(tokens)
    assertEquals(
      result,
      Right(
        TypeParserResult(
          TArrow(
            TVar(Identifier("a")),
            TVar(Identifier("b"))
          ),
          List()
        )
      )
    )
  }

  // test type with parenthesis
  // Int -> (Int -> Boolean) -> Boolean
  test("parse_type_with_parenthesis") {
    val tokens = List(
      IntKw,
      ArrowToken,
      LeftParenToken,
      IntKw,
      ArrowToken,
      BooleanKw,
      RightParenToken,
      ArrowToken,
      BooleanKw
    )
    val result = TypeParser.parseType(tokens)
    assertEquals(
      result,
      Right(
        TypeParserResult(
          TArrow(
            TInt,
            TArrow(
              TArrow(TInt, TBoolean),
              TBoolean
            )
          ),
          List()
        )
      )
    )
  }

