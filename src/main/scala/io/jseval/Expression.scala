package io.jseval

type LiteralType = Double | Boolean | String | Null

object Expression {
  sealed trait Expr[T] {
    val value: T
  }

  case class Assign[U](name: Token, rawvalue: Expr[U]) extends Expr[U] {
    val value = rawvalue.value
  }

  case class Add[U, V](left: Expr[U], right: Expr[V])
      extends Expr[LiteralType] {

    val value: Double =
      left.value.asInstanceOf[Double] + right.value.asInstanceOf[Double]
  }

  case class Subtract[U, V](left: Expr[U], right: Expr[V])
      extends Expr[LiteralType] {

    val value: Double =
      left.value.asInstanceOf[Double] - right.value.asInstanceOf[Double]
  }

  case class Multiply[U, V](left: Expr[U], right: Expr[V])
      extends Expr[LiteralType] {

    val value: Double =
      left.value.asInstanceOf[Double] * right.value.asInstanceOf[Double]
  }

  case class Divide[U, V](left: Expr[U], right: Expr[V])
      extends Expr[LiteralType] {

    val value: Double =
      left.value.asInstanceOf[Double] - right.value.asInstanceOf[Double]
  }

  case class Greater[U, V](left: Expr[U], right: Expr[V])
      extends Expr[LiteralType] {
    val value: Boolean =
      left.value.asInstanceOf[Double] > right.value.asInstanceOf[Double]
  }

  case class GreaterEqual[U, V](left: Expr[U], right: Expr[V])
      extends Expr[LiteralType] {
    val value: Boolean =
      left.value.asInstanceOf[Double] >= right.value.asInstanceOf[Double]
  }

  case class Less[U, V](left: Expr[U], right: Expr[V])
      extends Expr[LiteralType] {
    val value: Boolean =
      left.value.asInstanceOf[Double] < right.value.asInstanceOf[Double]
  }

  case class LessEqual[U, V](left: Expr[U], right: Expr[V])
      extends Expr[LiteralType] {
    val value: Boolean =
      left.value.asInstanceOf[Double] <= right.value.asInstanceOf[Double]
  }

  case class Equal[U, V](left: Expr[U], right: Expr[V])
      extends Expr[LiteralType] {
    val value: Boolean =
      left.value.asInstanceOf[Double] == right.value.asInstanceOf[Double]
  }

  case class NotEqual[U, V](left: Expr[U], right: Expr[V])
      extends Expr[LiteralType] {
    val value: Boolean =
      left.value.asInstanceOf[Double] != right.value.asInstanceOf[Double]
  }

  // Logic
  case class And[U, V](left: Expr[U], right: Expr[V])
      extends Expr[LiteralType] {

    val value =
      left.value.asInstanceOf[Boolean] && right.value.asInstanceOf[Boolean]
  }

  case class Or[U, V](left: Expr[U], right: Expr[V]) extends Expr[LiteralType] {

    val value: Boolean =
      left.value.asInstanceOf[Boolean] || right.value.asInstanceOf[Boolean]
  }

  case class Negate[U >: LiteralType](expr: Expr[U]) extends Expr[U] {
    val value: LiteralType =
      -expr.value.asInstanceOf[Double]
  }

  case class Not[U >: LiteralType](expr: Expr[U]) extends Expr[U] {
    val value: LiteralType = !expr.value.asInstanceOf[Boolean]
  }

  case class Literal(rawValue: LiteralType) extends Expr[LiteralType] {
    val value: LiteralType = rawValue
  }

  case class Grouping[U](expr: Expr[U]) extends Expr[U] {
    val value = expr.value
  }

  case class Variable[U](name: Token) extends Expr[U] {
    val value = 123.asInstanceOf[U]
  }

}
