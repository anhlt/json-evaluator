package io.jseval.parser

import cats._
import io.jseval.parser.{ParserOut, ParserResult}
import io.jseval.Token
import io.jseval.CompilerError
import io.jseval.Literal
import io.jseval.Expression
import io.jseval.Keyword
import io.jseval.Operator
import cats.implicits._
import io.jseval.Expression.BuildinModule.BuildinFn
import io.jseval.Expression.Buildin
import io.jseval.Expression.Variable
import io.jseval.Expression.{Expr, App}
import io.jseval.Expression.Abs
import io.jseval.TypModule.TAny
import io.jseval.Expression.Binding
import io.jseval.parser.Utils._
import io.jseval.TypModule.Typ

trait PrefixParser[T] {
  def parse[F[_]](
      tokens: List[Token]
  )(implicit
      a: MonadError[F, CompilerError]
  ): F[ParserResult[T]]
}

trait PrefixExprParser extends PrefixParser[Expr]

case object LiteralParser extends PrefixExprParser {

  def parse[F[_]](tokens: List[Token])(implicit
      a: MonadError[F, CompilerError]
  ): F[ParserResult[Expr]] = {
    tokens match
      case Literal.Number(l) :: rest =>
        a.pure(ParserOut(Expression.LiteralExpr(l.toInt), rest))
      case Literal.FloatNumber(l) :: rest =>
        a.pure(ParserOut(Expression.LiteralExpr(l.toDouble), rest))
      case Literal.Str(l) :: rest =>
        a.pure(ParserOut(Expression.LiteralExpr(l), rest))
      case Keyword.TrueKw :: rest =>
        a.pure(ParserOut(Expression.LiteralExpr(true), rest))
      case Keyword.FalseKw :: rest =>
        a.pure(ParserOut(Expression.LiteralExpr(false), rest))
      case _ => a.raiseError(CompilerError.ExpectExpression(tokens))
  }

}

/*
  IdentifierParser is the parser for identifier.
  - It could be simple variable
  - It could be function call caller(arg, *)
 */
case object IdentifierParser extends PrefixExprParser {
  def parse[F[_]](tokens: List[Token])(implicit
      a: MonadError[F, CompilerError]
  ): F[ParserResult[Expr]] = {
    tokens match
      case Literal.Identifier(name) :: rest =>
        val variableExpr = Expression.Variable(Literal.Identifier(name))
        rest match {
          case Operator.LeftParenToken :: tokensAfterLeft =>
            app(
              variableExpr,
              rest
            )
          case _ => a.pure(ParserOut(variableExpr, rest))
        }

      case _ => a.raiseError(CompilerError.ExpectExpression(tokens))
  }

  // app is the parser for function call
  // caller(arg, arg, arg)
  def app[F[_]](
      callerExpr: Expr,
      tokens: List[Token]
  )(implicit
      me: MonadError[F, CompilerError]
  ): F[ParserResult[Expr]] = {

    for {
      leftParenAndRmn <- consume(Operator.LeftParenToken, tokens)
      (_, rmnAfterLP) = leftParenAndRmn
      argAndRemaining <- appArgs(callerExpr, rmnAfterLP)
      rParenAndRmn <- consume(Operator.RightParenToken, argAndRemaining.rmn)
    } yield ParserOut(argAndRemaining.expr, rParenAndRmn._2)

  }

  def appArgs[F[_]](
      callerExpr: Expr,
      tokens: List[Token]
  )(implicit
      me: MonadError[F, CompilerError]
  ): F[ParserResult[Expr]] = {

    for {
      argAndRemaining <- ExpressionParser.expression(
        tokens,
        precedence = Precendence.LOGICAL_OR
      ) // could be conflix with tuple -> so we set precendece equal to nearest precedence which is OR
      nextResult <- (for {
        commaAndTokens <- consume(Operator.CommaToken, argAndRemaining.rmn)
        (comma, afterComma) = commaAndTokens
        rs <- appArgs(App(callerExpr, argAndRemaining.expr), afterComma)
      } yield (rs)).recover({
        case CompilerError.ExpectToken(Operator.CommaToken) =>
          ParserOut(App(callerExpr, argAndRemaining.expr), argAndRemaining.rmn)
      })

    } yield nextResult

  }
}

case object UnaryPrefixParser extends PrefixExprParser {
  def parse[F[_]](tokens: List[Token])(implicit
      a: MonadError[F, CompilerError]
  ): F[ParserResult[Expr]] = {
    tokens match
      case Operator.MinusToken :: rest =>
        for {
          parserOut <- ExpressionParser.parsePrecedence(Precendence.UNARY, rest)
        } yield ParserOut(
          Buildin(BuildinFn.Unary(BuildinFn.Negate, parserOut.expr)),
          parserOut.rmn
        )
      case Operator.BangToken :: rest =>
        for {
          parserOut <- ExpressionParser.parsePrecedence(Precendence.UNARY, rest)
        } yield ParserOut(
          Buildin(BuildinFn.Unary(BuildinFn.Not, parserOut.expr)),
          parserOut.rmn
        )
      case _ => a.raiseError(CompilerError.ExpectExpression(tokens))
  }
}

case object ParenthesisParser extends PrefixExprParser {
  def parse[F[_]](tokens: List[Token])(implicit
      a: MonadError[F, CompilerError]
  ): F[ParserResult[Expr]] = {

    tokens match
      case Operator.LeftParenToken :: rest =>
        parenBody(rest)
      case _ => a.raiseError(CompilerError.ExpectExpression(tokens))
  }
  // Parse the body within a pair of parentheses (the part after "(")
  def parenBody[F[_]](
      tokens: List[Token]
  )(implicit a: MonadError[F, CompilerError]): F[ParserResult[Expr]] =
    for {
      parserOut <- ExpressionParser.expression(tokens)
      rmn = parserOut.rmn
      result <- rmn match
        case Operator.RightParenToken :: rmn =>
          a.pure(ParserOut(parserOut.expr, rmn))
        case _ => a.raiseError(CompilerError.ExpectClosing(rmn))
    } yield (result)

}

case object ConditionPrefixParser extends PrefixExprParser {
  def parse[F[_]](tokens: List[Token])(implicit
      a: MonadError[F, CompilerError]
  ): F[ParserResult[Expr]] = {
    tokens match
      case Keyword.IfKw :: rest =>
        for {
          parserOut <- ExpressionParser.expression(rest)
          rmn = parserOut.rmn
          result <- rmn match
            case Keyword.ThenKw :: rmn =>
              for {
                parserOut2 <- ExpressionParser.expression(rmn)
                rmn2 = parserOut2.rmn
                result <- rmn2 match
                  case Keyword.ElseKw :: rmn2 =>
                    for {
                      parserOut3 <- ExpressionParser.expression(rmn2)
                      rmn3 = parserOut3.rmn
                    } yield ParserOut(
                      Expression.Cond(
                        parserOut.expr,
                        parserOut2.expr,
                        parserOut3.expr
                      ),
                      rmn3
                    )
                  case _ =>
                    a.raiseError(CompilerError.ExpectToken(Keyword.ThenKw))
              } yield (result)
            case _ => a.raiseError(CompilerError.ExpectToken(Keyword.ElseKw))
        } yield (result)
      case _ => a.raiseError(CompilerError.ExpectExpression(tokens))
  }
}

/*
object to parse function
  fun x -> if x == 0 then 1 else x * fact(x - 1)
  fun x y -> x + y
  fun (x: Int) (y: Int) -> x + y
 */

case object FunctionPrefixParser extends PrefixExprParser {
  def parse[F[_]](tokens: List[Token])(implicit
      a: MonadError[F, CompilerError]
  ): F[ParserResult[Expr]] = {
    tokens match
      case Keyword.FunKw :: rest =>
        for {
          parserOut <- lambda(rest)
        } yield (parserOut)
      case _ => a.raiseError(CompilerError.ExpectToken(Keyword.FunKw))
  }

  /*
  / parsing parameter part after fun keyword
  / it could be either
  / 1. variable with no type
  / 2. variable with type
   */
  def parameter[F[_]](
      tokens: List[Token]
  )(implicit
      me: MonadError[F, CompilerError]
  ): F[(ParserResult[Expr], Option[Typ])] = {

    tokens match {
      case Operator.LeftParenToken :: rest => {
        for {
          argAndRmn <- IdentifierParser.parse(rest)
          variable <- asVariable(argAndRmn.expr)
          colonAndRmn <- consume(Operator.ColonToken, argAndRmn.rmn)
          (colon, afterColon) = colonAndRmn
          expectType <- TypeParser.parseType(afterColon)
          rightParenAndRmn <- consume(Operator.RightParenToken, expectType.rmn)
          (rightParen, afterRightParen) = rightParenAndRmn
        } yield (ParserOut(variable, afterRightParen), Some(expectType.expr))
      }
      case _ => {
        for {
          argAndRmn <- IdentifierParser.parse(tokens)
          variable <- asVariable(argAndRmn.expr)
        } yield (ParserOut(variable, argAndRmn.rmn), None)
      }
    }
  }

  def lambda[F[_]](
      tokens: List[Token]
  )(implicit
      me: MonadError[F, CompilerError]
  ): F[ParserResult[Expr]] = {

    for {
      exprAndType <- parameter(tokens)
      (argAndRemaining, expectType) = exprAndType
      variable <- asVariable(argAndRemaining.expr)
      bodyAndRmn <- lambda(argAndRemaining.rmn).recoverWith(_ =>
        lamdaBody(argAndRemaining.rmn)
      )
    } yield ParserOut(
      Abs(
        variableName = variable,
        variableType = expectType,
        body = bodyAndRmn.expr
      ),
      bodyAndRmn.rmn
    )
  }

  def lamdaBody[F[_]](
      tokens: List[Token]
  )(implicit
      me: MonadError[F, CompilerError]
  ): F[ParserResult[Expr]] = {
    for {
      arrowAndRmn <- consume(Operator.ArrowToken, tokens)
      (arrow, rmn) = arrowAndRmn
      bodyExpr <- ExpressionParser.expression(rmn)
    } yield (bodyExpr)

  }

}

/*
case object for let binding
      |let z : int = 4
      |let u : int = 3
      |let sum = fun x y -> x + y
      |in sum(z, u)

 */
case object LetBindingPrefixParser extends PrefixExprParser {
  def parse[F[_]](tokens: List[Token])(implicit
      a: MonadError[F, CompilerError]
  ): F[ParserResult[Expr]] = {
    tokens match
      case Keyword.LetKw :: rest =>
        for {
          parserOut <- letBinding(rest)
        } yield (parserOut)
      case _ =>
        a.raiseError(CompilerError.ExpectToken(Keyword.LetKw))
  }

  /*
  / parsing variable after let keyword
  / it could be either
  / 1. variable with no type
  / 2. variable with type
   */
  def letVariable[F[_]](
      tokens: List[Token]
  )(implicit
      me: MonadError[F, CompilerError]
  ): F[(ParserResult[Expr], Option[Typ])] = {

    (for {
      argAndRmn <- IdentifierParser.parse(tokens)
      colonAndRmn <- consume(Operator.ColonToken, argAndRmn.rmn)
      (colon, afterColon) = colonAndRmn
      expectedType <- TypeParser.parseType(afterColon)
    } yield (
      ParserOut(argAndRmn.expr, expectedType.rmn),
      Some(expectedType.expr)
    )).recoverWith({
      case CompilerError.ExpectToken(Operator.ColonToken) => (
        IdentifierParser.parse(tokens).map((_, None))
      )
    })
  }

  def letBinding[F[_]](
      tokens: List[Token]
  )(implicit
      me: MonadError[F, CompilerError]
  ): F[ParserOut] = {

    for {
      isRecursive <- rec(tokens)
      (isRec, afterRec) = isRecursive
      variableAndExpectedType <- letVariable(afterRec)
      (variableExpr, expectedType) = variableAndExpectedType
      // check if expected type is provided when let is recursive
      _ <- (isRec, expectedType) match {
        case (true, None) => me.raiseError(CompilerError.ExpectTypeWhenLetIsRecursive(variableExpr.expr))
        case _ => ().pure[F]
      }

      variable <- asVariable(variableExpr.expr)
      equalAndRmn <- consume(Operator.EqualToken, variableExpr.rmn)
      exprAndRmn <- ExpressionParser.expression(equalAndRmn._2)
      result: ParserResult[Expr] <- (for {
        rs <- in(exprAndRmn.rmn)
      } yield rs).recoverWith({ case CompilerError.ExpectToken(Keyword.InKw) =>
        LetBindingPrefixParser.parse(exprAndRmn.rmn)
      })

    } yield ParserOut(
      Binding(
        isRec,
        variable,
        variableType = expectedType,
        exprAndRmn.expr,
        result.expr,
      ),
      result.rmn
    )
  }

  def rec[F[_]](
      tokens: List[Token]
  )(implicit
      a: MonadError[F, CompilerError]
  ): F[(Boolean, List[Token])] =
    tokens match
      case Keyword.RecKw :: rest => a.pure(true, rest)
      case _                     => a.pure(false, tokens)

  def in[F[_]](
      tokens: List[Token]
  )(implicit
      me: MonadError[F, CompilerError]
  ): F[ParserResult[Expr]] = {

    for {
      inAndRmn <- consume(Keyword.InKw, tokens)
      (_, rmn) = inAndRmn
      exprAndRmn <- ExpressionParser.expression(rmn)
    } yield exprAndRmn
  }
}

case object BracketPrefixParser extends PrefixExprParser {
  def parse[F[_]](tokens: List[Token])(implicit
      a: MonadError[F, CompilerError]
  ): F[ParserResult[Expr]] = ???
}

case object BracePrefixParser extends PrefixExprParser {
  def parse[F[_]](tokens: List[Token])(implicit
      a: MonadError[F, CompilerError]
  ): F[ParserResult[Expr]] = ???
}
