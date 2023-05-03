package io.jseval

import cats.implicits._
import munit.CatsEffectSuite
import Expression as Expr
import Expression._
import Expression.BuildinModule._
import Expression.BuildinModule.BuildinFn._
import Expression.ValueModule._
import TypModule._
import io.jseval.{Token, Literal}
import io.jseval.typeinfer.TypeInfer
import io.jseval.parser.ExpressionParser

class NewTypeInferTest extends munit.FunSuite:

  type MyEither[A] = Either[CompilerError, A]

  val tokenX = Literal.Identifier("x")
  val tokenY = Literal.Identifier("y")
  val tokenZ = Literal.Identifier("z")
  val tokenU = Literal.Identifier("u")
  val tokenFactorial = Literal.Identifier("factorial")



  val x = Expr.Variable(tokenX)
  val y = Expr.Variable(tokenY)
  val tokenSum = Literal.Identifier("sum")


  implicit val env: TypeInfer.TypeEnv = Map()



  // test int plus double

  test("type_int_plus_double") {

    val input = """
   |1 + 2.0
   """.stripMargin

    val parserResult = for {
      tokens <- Scanner.parse(input)
      expr <- ExpressionParser.expression(tokens)
      outputType <- TypeInfer.infer(expr.expr)
    } yield outputType

    assertEquals(parserResult, Right(TDouble))
  }

  // test double plus int
  test("type_double_plus_int") {

    val input = """
   |1.0 + 2
   """.stripMargin

    val parserResult = for {
      tokens <- Scanner.parse(input)
      expr <- ExpressionParser.expression(tokens)
      outputType <- TypeInfer.infer(expr.expr)
    } yield outputType

    assertEquals(parserResult, Right(TDouble))
  }

  // test function call
  test("type_function_call") {

    val input = """
    |let sum = fun (x: int) (y : int) -> x + y  
    |in sum(1, 2)
   """.stripMargin

    val parserResult = for {
      tokens <- Scanner.parse(input)
      expr <- ExpressionParser.expression(tokens)
      outputType <- TypeInfer.infer(expr.expr)
    } yield (expr, outputType)


    // expected expr should be bind expr
    val expectedExpr = Binding(
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

    assertEquals(parserResult.map(_._1.expr), Right(expectedExpr))
    assertEquals(parserResult.map(_._2), Right(TInt))
  }

  // test recursive function call factorial number
  test("type_recursive_function_call_factorial_number") {

    val input = """
    |let rec factorial : int -> int = fun (x: int) -> if x == 1 then 1 else x * factorial(x - 1)  
    |in factorial(5)
   """.stripMargin

    val parserResult = for {
      tokens <- Scanner.parse(input)
      expr <- ExpressionParser.expression(tokens)
      outputType <- TypeInfer.infer(expr.expr)
    } yield (expr, outputType)

    // expected expr
    val expectedExpr = Binding(
      recursive = true,
      Variable(tokenFactorial),
      variableType = Some(TArrow(TInt, TInt)),
      Abs(
        Variable(tokenX),
        Some(TInt),
        Cond(
          Buildin(
            BuildinFn.Comparison(
              BuildinFn.Equal,
              Variable(tokenX),
              LiteralExpr(1)
            )
          ),
          LiteralExpr(1),
          Buildin(
            BuildinFn.Arithmetic(
              BuildinFn.Multiply,
              Variable(tokenX),
              App(
                Variable(tokenFactorial),
                Buildin(
                  BuildinFn.Arithmetic(
                    BuildinFn.Subtract,
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
        Variable(tokenFactorial),
        LiteralExpr(5)
      )
    )

    assertEquals(parserResult.map(_._1.expr), Right(expectedExpr))
    assertEquals(parserResult.map(_._2), Right(TInt))

  }