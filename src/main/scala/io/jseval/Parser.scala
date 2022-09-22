package io.jseval

import cats._
import cats.implicits._
import Expression._
import io.jseval.Expression
import io.jseval.Expression.BuildinModule.BuildinFn
import cats.instances._
import io.jseval.TypModule._
import scala.language.experimental

object Parser {

  enum Error(msg: String):
    case ExpectExpression(tokens: List[Token]) extends Error("ExpectExpression")
    case ExpectToken(token: Token) extends Error(s"Expected Token in $token")
    case ExpectTokens(tokens: List[Token])
        extends Error(s"Expected Token in $tokens")

    case ExpectIdentifer() extends Error(s"Expected Identifier")

    case ExpectClosing(tokens: List[Token])
        extends Error("Expect ')' after expression")
    case InvalidAssignmentTartget(token: Token)
        extends Error("Invalid assignment target.")

  def parseExpr[F[_]](ts: List[Token])(implicit
      a: MonadError[F, Error]
  ): F[ParserOut] = expression(ts)

  def expression[F[_]](
      tokens: List[Token]
  )(implicit a: MonadError[F, Error]): F[ParserOut] = or(tokens)

  // x (a, b, c)

  def app[F[_]](
      tokens: List[Token]
  )(implicit me: MonadError[F, Error]): F[ParserOut] = {

    for {
      bodyAndRmn <- expression(tokens)
      (body, rmn) = bodyAndRmn
      leftParenAndRmn <- consume(Operator.LeftParen, rmn)
      (_, rmnAfterLP) = leftParenAndRmn
      argAndRemaining <- appArgs(body, rmnAfterLP)
      (arg, rmnAfterArg) = argAndRemaining
      rParenAndRmn <- consume(Operator.RightParen, rmnAfterArg)
    } yield (arg, rParenAndRmn._2)

  }

  def appArgs[F[_]](
      previousExpr: Expr,
      tokens: List[Token]
  )(implicit me: MonadError[F, Error]): F[ParserOut] = {

    for {
      argAndRemaining <- expression(tokens)
      (arg, rmn) = argAndRemaining
      nextResult <- (for {
        commaAndTokens <- consume(Operator.Comma, rmn)
        (comma, afterComma) = commaAndTokens
        rs <- appArgs(App(previousExpr, arg), afterComma)
      } yield (rs)).recover({ case Error.ExpectToken(Operator.Comma) =>
        (App(previousExpr, arg), rmn)
      })

    } yield nextResult

  }

  def lambdaFunc[F[_]](
      tokens: List[Token]
  )(implicit me: MonadError[F, Error]): F[ParserOut] = {

    for {
      funTokensAndRemaining <- consume(Keyword.Fun, tokens)
      (funToken, rmn) = funTokensAndRemaining
      result <- lambda(rmn)
    } yield (result)
  }

  def lambda[F[_]](
      tokens: List[Token]
  )(implicit me: MonadError[F, Error]): F[ParserOut] = {

    for {
      // funTokensAndRemaining <- consume(Keyword.Fun, tokens)
      // (funToken, rmn) = tokens
      argAndRemaining <- identifier(tokens)
      (ident, afterIdent) = argAndRemaining
      variable <- asVariable(ident)
      bodyAndRmn <- lambda(afterIdent).recoverWith(_ => lamdaBody(afterIdent))
      // .recover(_ => expression(afterIdent))
      (bodyExpr, afterLambda) = bodyAndRmn

    } yield (
      Abs(variableName = variable, variableType = TAny, body = bodyExpr),
      afterLambda
    )
  }

  def lamdaBody[F[_]](
      tokens: List[Token]
  )(implicit me: MonadError[F, Error]): F[ParserOut] = {
    for {
      arrowAndRmn <- consume(Operator.Arrow, tokens)
      (arrow, rmn) = arrowAndRmn
      bodyExpr <- expression(rmn)
    } yield (bodyExpr)

  }

  def or[F[_]](
      tokens: List[Token]
  )(implicit a: MonadError[F, Error]): F[ParserOut] =
    binary(orOp, and)(tokens)

  def and[F[_]](
      tokens: List[Token]
  )(implicit a: MonadError[F, Error]): F[ParserOut] =
    binary(andOp, equality)(tokens)

  // Parse binary expressions that share this grammar
  // ```
  //    expr   -> descendant (OPERATOR descendant)  *
  // ```
  // Consider "equality" expression as an example. Its direct descendant is "comparison"
  // and its OPERATOR is ("==" | "!=").

  def binary[F[_]](
      op: Token => F[(Expr, Expr) => Expr],
      descendantFn: List[Token] => F[ParserOut]
  )(
      tokens: List[Token]
  )(implicit a: MonadError[F, Error]): F[ParserOut] =

    def matchOp(ts: List[Token], leftExpr: Expr): F[ParserOut] =
      ts match
        case token :: rest =>
          op(token)
            .flatMap(fn => {
              for {
                result <- descendantFn(rest)
                (rightExpr, rmn) = result
                continueResult <- matchOp(rmn, fn(leftExpr, rightExpr))
              } yield continueResult
            })
            .orElse(a.pure(leftExpr, ts))

        case _ => a.pure(leftExpr, ts)

    descendantFn(tokens).flatMap((expr, rest) => matchOp(rest, expr))

  def equality[F[_]](
      tokens: List[Token]
  )(implicit a: MonadError[F, Error]): F[ParserOut] =
    binary(equalityOp, comparison)(tokens)

  def comparison[F[_]](
      tokens: List[Token]
  )(implicit a: MonadError[F, Error]): F[ParserOut] =
    binary(comparisonOp, term)(tokens)

  def term[F[_]](
      tokens: List[Token]
  )(implicit a: MonadError[F, Error]): F[ParserOut] =
    binary(termOp, factor)(tokens)

  def factor[F[_]](
      tokens: List[Token]
  )(implicit a: MonadError[F, Error]): F[ParserOut] =
    binary(factorOp, unary)(tokens)

  def unary[F[_]](
      tokens: List[Token]
  )(implicit a: MonadError[F, Error]): F[ParserOut] =
    tokens match {
      case token :: rest =>
        (for {
          fn <- unaryOp(token)
          result <- unary(rest)
          (expr, rmn) = result
        } yield (fn(expr), rmn)).recoverWith(_ =>
          primaryOrIdentiferOrGroup(tokens)
        )

      case _ =>
        primaryOrIdentiferOrGroup(tokens)
    }

  def primaryOrIdentiferOrGroup[F[_]](
      tokens: List[Token]
  )(implicit a: MonadError[F, Error]): F[ParserOut] =
    primary(tokens).orElse(identifier(tokens)).orElse(group(tokens))

  def primary[F[_]](
      tokens: List[Token]
  )(implicit a: MonadError[F, Error]): F[ParserOut] =
    tokens match
      case Literal.Number(l) :: rest =>
        a.pure(Expression.LiteralExpr(l.toDouble), rest)
      case Literal.Str(l) :: rest => a.pure(Expression.LiteralExpr(l), rest)
      case Keyword.True :: rest   => a.pure(Expression.LiteralExpr(true), rest)
      case Keyword.False :: rest  => a.pure(Expression.LiteralExpr(false), rest)
      case _ => a.raiseError(Error.ExpectExpression(tokens))

  def identifier[F[_]](
      tokens: List[Token]
  )(implicit a: MonadError[F, Error]): F[ParserOut] =
    tokens match {
      case Literal.Identifier(name) :: rest =>
        a.pure(Expression.Variable(Literal.Identifier(name)), rest)
      case _ => a.raiseError(Error.ExpectExpression(tokens))
    }

  def asVariable[F[_]](x: Expr)(implicit
      a: MonadError[F, Error]
  ): F[Variable] = {
    x.match {
      case v: Variable => a.pure(v)
      case _           => a.raiseError(Error.ExpectIdentifer())
    }

  }

  def group[F[_]](
      tokens: List[Token]
  )(implicit a: MonadError[F, Error]): F[ParserOut] = {
    tokens match
      case Operator.LeftParen :: rest => parenBody(rest)
      case _ => a.raiseError(Error.ExpectExpression(tokens))

  }

  // Parse the body within a pair of parentheses (the part after "(")
  def parenBody[F[F]](
      tokens: List[Token]
  )(implicit a: MonadError[F, Error]): F[ParserOut] =
    expression(tokens).flatMap((expr, rest) =>
      rest match
        case Operator.RightParen :: rmn =>
          a.pure(Expression.Grouping(expr), rmn)
        case _ => a.raiseError(Error.ExpectClosing(rest))
    )

  def consume[F[_]](
      expect: Token,
      tokens: List[Token]
  )(implicit a: MonadError[F, Error]): F[(Token, List[Token])] =
    tokens.headOption match {
      case Some(token) => {
        if (token == expect) {
          a.pure((expect, tokens.tail))
        } else {
          a.raiseError(Error.ExpectToken(expect))
        }
      }
      case _ => a.raiseError(Error.ExpectToken(expect))
    }

  type ParserOut = (Expr, List[Token])

  def orOp[F[_]](token: Token)(implicit
      me: MonadError[F, Error]
  ): F[(Expr, Expr) => Expr] =
    token match
      case Keyword.Or =>
        me.pure((l, r) => Buildin(BuildinFn.Logical(BuildinFn.Or, l, r)))
      case _ =>
        me.raiseError(
          Error.ExpectToken(
            Keyword.Or
          )
        )

  def andOp[F[_]](token: Token)(implicit
      me: MonadError[F, Error]
  ): F[(Expr, Expr) => Expr] =
    token match
      case Keyword.And =>
        me.pure((l, r) => Buildin(BuildinFn.Logical(BuildinFn.And, l, r)))
      case _ =>
        me.raiseError(
          Error.ExpectToken(
            Keyword.And
          )
        )

  def equalityOp[F[_]](token: Token)(implicit
      me: MonadError[F, Error]
  ): F[(Expr, Expr) => Expr] =
    token match
      case Operator.EqualEqual =>
        me.pure((l, r) => Buildin(BuildinFn.Comparison(BuildinFn.Equal, l, r)))
      case Operator.BangEqual =>
        me.pure((l, r) =>
          Buildin(BuildinFn.Comparison(BuildinFn.NotEqual, l, r))
        )
      case _ =>
        me.raiseError(
          Error.ExpectTokens(
            List(
              Operator.EqualEqual,
              Operator.BangEqual
            )
          )
        )

  def comparisonOp[F[_]](token: Token)(implicit
      me: MonadError[F, Error]
  ): F[(Expr, Expr) => Expr] =
    token match
      case Operator.Less =>
        me.pure((l, r) => Buildin(BuildinFn.Comparison(BuildinFn.Less, l, r)))
      case Operator.LessEqual =>
        me.pure((l, r) =>
          Buildin(BuildinFn.Comparison(BuildinFn.LessEqual, l, r))
        )
      case Operator.Greater =>
        me.pure((l, r) =>
          Buildin(BuildinFn.Comparison(BuildinFn.Greater, l, r))
        )
      case Operator.GreaterEqual =>
        me.pure((l, r) =>
          Buildin(BuildinFn.Comparison(BuildinFn.GreaterEqual, l, r))
        )
      case _ =>
        me.raiseError(
          Error.ExpectTokens(
            List(
              Operator.Less,
              Operator.LessEqual,
              Operator.Greater,
              Operator.GreaterEqual
            )
          )
        )

  def termOp[F[_]](token: Token)(implicit
      me: MonadError[F, Error]
  ): F[(Expr, Expr) => Expr] =
    token match
      case Operator.Plus =>
        me.pure((l, r) => Buildin(BuildinFn.Arthimetric(BuildinFn.Add, l, r)))

      case Operator.Minus =>
        me.pure((l, r) => Buildin(BuildinFn.Arthimetric(BuildinFn.Sub, l, r)))

      case _ =>
        me.raiseError(Error.ExpectTokens(List(Operator.Plus, Operator.Minus)))

  def factorOp[F[_]](token: Token)(implicit
      me: MonadError[F, Error]
  ): F[(Expr, Expr) => Expr] =
    token match

      case Operator.Star =>
        me.pure((l, r) => Buildin(BuildinFn.Arthimetric(BuildinFn.Mul, l, r)))
      case Operator.Slash =>
        me.pure((l, r) => Buildin(BuildinFn.Arthimetric(BuildinFn.Div, l, r)))
      case _ =>
        me.raiseError(Error.ExpectTokens(List(Operator.Star, Operator.Slash)))

  def unaryOp[F[_]](token: Token)(implicit
      me: MonadError[F, Error]
  ): F[Expr => Expr] =
    token match
      case x @ Operator.Minus =>
        me.pure(x => Buildin(BuildinFn.Unary(BuildinFn.Negate, x)))
      case x @ Operator.Bang =>
        me.pure(x => Buildin(BuildinFn.Unary(BuildinFn.Not, x)))
      case _ =>
        me.raiseError(Error.ExpectTokens(List(Operator.Minus, Operator.Bang)))

}
