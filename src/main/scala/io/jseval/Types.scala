package io.jseval

import Expression._
import Expression.BuildinModule._
import Expression.BuildinModule.BuildinFn._
import Expression.ValueModule._

object TypModule {
  sealed trait Typ

  sealed trait BaseTyp extends Typ

  case object TInt extends BaseTyp
  case object TDouble extends BaseTyp
  case object TString extends BaseTyp
  case object TBoolean extends BaseTyp

  case object TAny extends Typ
  case object TUnit extends Typ

  case class TArrow(argType: Typ, bodyType: Typ) extends Typ
  case class TProduct(firstType: Typ, secondType: Typ) extends Typ

  case class TGeneric(name: Token) extends Typ

}
