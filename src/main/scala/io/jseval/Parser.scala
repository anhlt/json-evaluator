package io.jseval

import cats._
import cats.implicits._
import Expression._
import io.jseval.Expression
import io.jseval.Expression.BuildinFn
import cats.instances._

object Parser {

  enum Error(msg: String, tokens: List[Token]):
    case ExpectExpression(tokens: List[Token])
        extends Error("ExpectExpression", tokens)
    case ExpectClosing(tokens: List[Token])
        extends Error("Expect ')' after expression", tokens)
    case InvalidAssignmentTartget(token: Token)
        extends Error("Invalid assignment target.", List(token))

  def parseExpression[F[_]](tokens: List[Token])(implicit
      ae: ApplicativeError[F, Error]
  ): F[Expr] = ???

  def parse(ts: List[Token]): ExprParser = expression(ts)

  def expression: List[Token] => ExprParser = or

  def or: List[Token] => ExprParser = binary(orOp, and)

  def and: List[Token] => ExprParser = binary(andOp, equality)

  // Parse binary expressions that share this grammar
  // ```
  //    expr   -> descendant (OPERATOR descendant)  *
  // ```
  // Consider "equality" expression as an example. Its direct descendant is "comparison"
  // and its OPERATOR is ("==" | "!=").
  def binary(
      op: BinaryOp,
      descendant: List[Token] => ExprParser
  )(
      tokens: List[Token]
  ): ExprParser =

    def matchOp(ts: List[Token], l: Expr): ExprParser =
      ts match
        case token :: rest =>
          op(token) match
            case Some(fn) =>
              descendant(rest).flatMap((r: Expr, rmn: List[Token]) =>
                matchOp(rmn, fn(l, r))
              )
            case None => Right(l, ts)
        case _ => Right(l, ts)

    descendant(tokens).flatMap((expr, rest) => matchOp(rest, expr))

  def equality: List[Token] => ExprParser =
    binary(equalityOp, comparison)
  def comparison: List[Token] => ExprParser =
    binary(comparisonOp, term)
  def term: List[Token] => ExprParser = binary(termOp, factor)
  def factor: List[Token] => ExprParser =
    binary(factorOp, unary)

  def unary(tokens: List[Token]): ExprParser =
    tokens match
      case token :: rest =>
        unaryOp(token) match
          case Some(fn) =>
            unary(rest).flatMap((expr, rmn) => Right(fn(expr), rmn))
          case None => primary(tokens)
      case _ => primary(tokens)

  def primary(tokens: List[Token]): ExprParser =
    tokens match
      case Literal.Number(l) :: rest =>
        Right(Expression.LiteralExpr(l.toDouble), rest)
      case Literal.Str(l) :: rest => Right(Expression.LiteralExpr(l), rest)
      case Keyword.True :: rest   => Right(Expression.LiteralExpr(true), rest)
      case Keyword.False :: rest  => Right(Expression.LiteralExpr(false), rest)
      case Keyword.Null :: rest   => Right(Expression.LiteralExpr(null), rest)
      case Literal.Identifier(name) :: rest =>
        Right(Expression.Variable(Literal.Identifier(name)), rest)
      case Operator.LeftParen :: rest => parenBody(rest)
      case _                          => Left(Error.ExpectExpression(tokens))

  // Parse the body within a pair of parentheses (the part after "(")
  def parenBody(
      tokens: List[Token]
  ): ExprParser = expression(tokens).flatMap((expr, rest) =>
    rest match
      case Operator.RightParen :: rmn => Right(Expression.Grouping(expr), rmn)
      case _                          => Left(Error.ExpectClosing(rest))
  )

  def consume(
      expect: Token,
      tokens: List[Token]
  ): Either[Error, (Token, List[Token])] =
    tokens.headOption match {
      case Some(expect) => Right(expect, tokens.tail)
      case _            => Left(Error.ExpectExpression(tokens))
    }


  type ExprParser = Either[Error, (Expr, List[Token])]

  type BinaryOp = Token => Option[(Expr, Expr) => Expr]

  type UnaryOp = Token => Option[Expr => Expr]

  val orOp: BinaryOp =
    case Keyword.Or =>
      Some((l, r) => Buildin(BuildinFn.Logical(BuildinFn.Or, l, r)))
    case _ => None

  val andOp: BinaryOp =
    case Keyword.And =>
      Some((l, r) => Buildin(BuildinFn.Logical(BuildinFn.And, l, r)))
    case _ => None

  val equalityOp: BinaryOp =
    case Operator.EqualEqual =>
      Some((l, r) => Buildin(BuildinFn.Comparison(BuildinFn.Equal, l, r)))
    case Operator.BangEqual =>
      Some((l, r) => Buildin(BuildinFn.Comparison(BuildinFn.NotEqual, l, r)))
    case _ => None

  val comparisonOp: BinaryOp =
    case Operator.Less =>
      Some((l, r) => Buildin(BuildinFn.Comparison(BuildinFn.Less, l, r)))
    case Operator.LessEqual =>
      Some((l, r) => Buildin(BuildinFn.Comparison(BuildinFn.LessEqual, l, r)))
    case Operator.Greater =>
      Some((l, r) => Buildin(BuildinFn.Comparison(BuildinFn.Greater, l, r)))
    case Operator.GreaterEqual =>
      Some((l, r) =>
        Buildin(BuildinFn.Comparison(BuildinFn.GreaterEqual, l, r))
      )
    case _ => None

  val termOp: BinaryOp =
    case Operator.Plus =>
      Some((l, r) => Buildin(BuildinFn.Arthimetric(BuildinFn.Add, l, r)))

    case Operator.Minus =>
      Some((l, r) => Buildin(BuildinFn.Arthimetric(BuildinFn.Sub, l, r)))

    case _ => None

  val factorOp: BinaryOp =
    case Operator.Star =>
      Some((l, r) => Buildin(BuildinFn.Arthimetric(BuildinFn.Mul, l, r)))
    case Operator.Slash =>
      Some((l, r) => Buildin(BuildinFn.Arthimetric(BuildinFn.Div, l, r)))
    case _ => None

  val unaryOp: UnaryOp =
    case Operator.Minus =>
      Some(x => Buildin(BuildinFn.Unary(BuildinFn.Negate, x)))
    case Operator.Bang => Some(x => Buildin(BuildinFn.Unary(BuildinFn.Not, x)))
    case _             => None

}
