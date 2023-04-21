package io.jseval.parser

import cats.MonadError
import io.jseval.Parser.ParserOut
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
import java.security.Identity
import io.jseval.Expression.Abs
import io.jseval.TypModule.TAny
import io.jseval.Expression.Binding
import io.jseval.parser.Utils._

trait PrefixParser {
  def parse[F[_]](
      tokens: List[Token]
  )(implicit a: MonadError[F, CompilerError], JSParser: JSParser): F[ParserOut]

}

case object LiteralParser extends PrefixParser {

  def parse[F[_]](tokens: List[Token])(implicit
      a: MonadError[F, CompilerError],
      JSParser: JSParser
  ): F[ParserOut] = {
    tokens match
      case Literal.Number(l) :: rest =>
        a.pure(ParserOut(Expression.LiteralExpr(l.toDouble), rest))
      case Literal.Str(l) :: rest =>
        a.pure(ParserOut(Expression.LiteralExpr(l), rest))
      case Keyword.True :: rest =>
        a.pure(ParserOut(Expression.LiteralExpr(true), rest))
      case Keyword.False :: rest =>
        a.pure(ParserOut(Expression.LiteralExpr(false), rest))
      case _ => a.raiseError(CompilerError.ExpectExpression(tokens))
  }

}

/*
  IdentifierParser is the parser for identifier.
  - It could be simple variable
  - It could be function call caller(arg, *)
 */
case object IdentifierParser extends PrefixParser {
  def parse[F[_]](tokens: List[Token])(implicit
      a: MonadError[F, CompilerError],
      JSParser: JSParser
  ): F[ParserOut] = {
    tokens match
      case Literal.Identifier(name) :: rest =>
        val variableExpr = Expression.Variable(Literal.Identifier(name))
        rest match {
          case Operator.LeftParen :: tokensAfterLeft =>
            app(
              variableExpr,
              rest
            )
          case _ => a.pure(ParserOut(variableExpr, rest))
        }

      case _ => a.raiseError(CompilerError.ExpectExpression(tokens))
  }

  def app[F[_]](
      callerExpr: Expr,
      tokens: List[Token]
  )(implicit
      me: MonadError[F, CompilerError],
      jsParser: JSParser
  ): F[ParserOut] = {

    for {
      leftParenAndRmn <- consume(Operator.LeftParen, tokens)
      (_, rmnAfterLP) = leftParenAndRmn
      argAndRemaining <- appArgs(callerExpr, rmnAfterLP)
      rParenAndRmn <- consume(Operator.RightParen, argAndRemaining.rmn)
    } yield ParserOut(argAndRemaining.expr, rParenAndRmn._2)

  }

  def appArgs[F[_]](
      callerExpr: Expr,
      tokens: List[Token]
  )(implicit
      me: MonadError[F, CompilerError],
      jsParser: JSParser
  ): F[ParserOut] = {

    for {
      argAndRemaining <- jsParser.expression(
        tokens,
        precedence = Precendence.LOGICAL_OR
      ) // could be conflix with tuple -> so we set precendece equal to nearest precedence which is OR
      nextResult <- (for {
        commaAndTokens <- consume(Operator.Comma, argAndRemaining.rmn)
        (comma, afterComma) = commaAndTokens
        rs <- appArgs(App(callerExpr, argAndRemaining.expr), afterComma)
      } yield (rs)).recover({ case CompilerError.ExpectToken(Operator.Comma) =>
        ParserOut(App(callerExpr, argAndRemaining.expr), argAndRemaining.rmn)
      })

    } yield nextResult

  }
}

case object UnaryPrefixParser extends PrefixParser {
  def parse[F[_]](tokens: List[Token])(implicit
      a: MonadError[F, CompilerError],
      JSParser: JSParser
  ): F[ParserOut] = {
    tokens match
      case Operator.Minus :: rest =>
        for {
          parserOut <- JSParser.parsePrecedence(Precendence.UNARY, rest)
        } yield ParserOut(
          Buildin(BuildinFn.Unary(BuildinFn.Negate, parserOut.expr)),
          parserOut.rmn
        )
      case Operator.Bang :: rest =>
        for {
          parserOut <- JSParser.parsePrecedence(Precendence.UNARY, rest)
        } yield ParserOut(
          Buildin(BuildinFn.Unary(BuildinFn.Not, parserOut.expr)),
          parserOut.rmn
        )
      case _ => a.raiseError(CompilerError.ExpectExpression(tokens))
  }
}

case object ParenthesisParser extends PrefixParser {
  def parse[F[_]](tokens: List[Token])(implicit
      a: MonadError[F, CompilerError],
      JSParser: JSParser
  ): F[ParserOut] = {

    tokens match
      case Operator.LeftParen :: rest =>
        parenBody(rest, JSParser)
      case _ => a.raiseError(CompilerError.ExpectExpression(tokens))
  }
  // Parse the body within a pair of parentheses (the part after "(")
  def parenBody[F[_]](
      tokens: List[Token],
      JSParser: JSParser
  )(implicit a: MonadError[F, CompilerError]): F[ParserOut] =
    for {
      parserOut <- JSParser.expression(tokens)
      rmn = parserOut.rmn
      result <- rmn match
        case Operator.RightParen :: rmn =>
          a.pure(ParserOut(parserOut.expr, rmn))
        case _ => a.raiseError(CompilerError.ExpectClosing(rmn))
    } yield (result)

}

case object ConditionPrefixParser extends PrefixParser {
  def parse[F[_]](tokens: List[Token])(implicit
      a: MonadError[F, CompilerError],
      JSParser: JSParser
  ): F[ParserOut] = {
    tokens match
      case Keyword.If :: rest =>
        for {
          parserOut <- JSParser.expression(rest)
          rmn = parserOut.rmn
          result <- rmn match
            case Keyword.Then :: rmn =>
              for {
                parserOut2 <- JSParser.expression(rmn)
                rmn2 = parserOut2.rmn
                result <- rmn2 match
                  case Keyword.Else :: rmn2 =>
                    for {
                      parserOut3 <- JSParser.expression(rmn2)
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
                    a.raiseError(CompilerError.ExpectToken(Keyword.Then))
              } yield (result)
            case _ => a.raiseError(CompilerError.ExpectToken(Keyword.Else))
        } yield (result)
      case _ => a.raiseError(CompilerError.ExpectExpression(tokens))
  }
}

/*
object to parse function
  fun x -> if x == 0 then 1 else x * fact(x - 1)
  fun x y -> x + y
 */

case object FunctionPrefixParser extends PrefixParser {
  def parse[F[_]](tokens: List[Token])(implicit
      a: MonadError[F, CompilerError],
      JSParser: JSParser
  ): F[ParserOut] = {
    tokens match
      case Keyword.Fun :: rest =>
        for {
          parserOut <- lambda(rest)
        } yield (parserOut)
      case _ => a.raiseError(CompilerError.ExpectToken(Keyword.Fun))
  }

  def lambda[F[_]](
      tokens: List[Token]
  )(implicit
      me: MonadError[F, CompilerError],
      JSParser: JSParser
  ): F[ParserOut] = {

    for {
      argAndRemaining <- IdentifierParser.parse(tokens)
      variable <- asVariable(argAndRemaining.expr)
      bodyAndRmn <- lambda(argAndRemaining.rmn).recoverWith(_ =>
        lamdaBody(argAndRemaining.rmn)
      )

    } yield ParserOut(
      Abs(variableName = variable, variableType = TAny, body = bodyAndRmn.expr),
      bodyAndRmn.rmn
    )
  }

  def lamdaBody[F[_]](
      tokens: List[Token]
  )(implicit
      me: MonadError[F, CompilerError],
      JSParser: JSParser
  ): F[ParserOut] = {
    for {
      arrowAndRmn <- consume(Operator.Arrow, tokens)
      (arrow, rmn) = arrowAndRmn
      bodyExpr <- JSParser.expression(rmn)
    } yield (bodyExpr)

  }

}

/*
case object for let binding
      |let z = 4
      |let u = 3
      |let sum = fun x y -> x + y
      |in sum(z, u)

 */
case object LetBindingPrefixParser extends PrefixParser {
  def parse[F[_]](tokens: List[Token])(implicit
      a: MonadError[F, CompilerError],
      JSParser: JSParser
  ): F[ParserOut] = {
    tokens match
      case Keyword.Let :: rest =>
        for {
          parserOut <- letBinding(rest)
        } yield (parserOut)
      case _ =>
        println("Let Parser tokens: " + tokens)
        a.raiseError(CompilerError.ExpectToken(Keyword.Let))
  }

  def letBinding[F[_]](
      tokens: List[Token]
  )(implicit
      me: MonadError[F, CompilerError],
      JSParser: JSParser
  ): F[ParserOut] = {

    for {
      isRecursive <- rec(tokens)
      (isRec, afterRec) = isRecursive
      _ = println("afterRec: " + afterRec)
      identiferAndRmn <- IdentifierParser.parse(afterRec)
      variable <- asVariable(identiferAndRmn.expr)
      equalAndRmn <- consume(Operator.Equal, identiferAndRmn.rmn)
      exprAndRmn <- JSParser.expression(equalAndRmn._2)
      _ = println("expr and rmn" + exprAndRmn)
      result <- (for {
        rs <- in(exprAndRmn.rmn)
      } yield rs).recoverWith({ case CompilerError.ExpectToken(Keyword.In) =>
        LetBindingPrefixParser.parse(exprAndRmn.rmn)
      })

    } yield ParserOut(
      Binding(isRec, variable, exprAndRmn.expr, result._1),
      result._2
    )
  }

  def rec[F[_]](
      tokens: List[Token]
  )(implicit
      a: MonadError[F, CompilerError],
      JSParser: JSParser
  ): F[(Boolean, List[Token])] =
    tokens match
      case Keyword.Rec :: rest => a.pure(true, rest)
      case _                   => a.pure(false, tokens)

  def in[F[_]](
      tokens: List[Token]
  )(implicit
      me: MonadError[F, CompilerError],
      JSParser: JSParser
  ): F[ParserOut] = {

    for {
      inAndRmn <- consume(Keyword.In, tokens)
      (_, rmn) = inAndRmn
      exprAndRmn <- JSParser.expression(rmn)
    } yield exprAndRmn
  }
}

case object BracketPrefixParser extends PrefixParser {
  def parse[F[_]](tokens: List[Token])(implicit
      a: MonadError[F, CompilerError],
      JSParser: JSParser
  ): F[ParserOut] = ???
}

case object BracePrefixParser extends PrefixParser {
  def parse[F[_]](tokens: List[Token])(implicit
      a: MonadError[F, CompilerError],
      JSParser: JSParser
  ): F[ParserOut] = ???
}
