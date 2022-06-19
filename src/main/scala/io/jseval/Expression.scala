package io.jseval

type LiteralType = Double | Boolean | String | Null

object Expression {
  sealed trait Expr

  case class Assign(name: Token, rawvalue: Expr) extends Expr

  case class Add(left: Expr, right: Expr) extends Expr

  case class Subtract(left: Expr, right: Expr) extends Expr

  case class Multiply(left: Expr, right: Expr) extends Expr

  case class Divide(left: Expr, right: Expr) extends Expr

  case class Greater(left: Expr, right: Expr) extends Expr

  case class GreaterEqual(left: Expr, right: Expr) extends Expr

  case class Less(left: Expr, right: Expr) extends Expr

  case class LessEqual(left: Expr, right: Expr) extends Expr

  case class Equal(left: Expr, right: Expr) extends Expr

  case class NotEqual(left: Expr, right: Expr) extends Expr

  // Logic
  case class And(left: Expr, right: Expr) extends Expr

  case class Or(left: Expr, right: Expr) extends Expr

  case class Negate(expr: Expr) extends Expr

  case class Not(expr: Expr) extends Expr

  case class LiteralExpr(value: LiteralType) extends Expr

  case class Grouping(expr: Expr) extends Expr

  case class Variable(name: Token) extends Expr

}
