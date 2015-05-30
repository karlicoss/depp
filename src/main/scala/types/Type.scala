package types

import util.PrettyPrintable

/**
 * Created by karlicos on 29.05.15.
 */

sealed abstract class Type extends PrettyPrintable{
  def to(other: Type): Arrow = Arrow(this, other)
}

//final case class Sum(types: Array[Type]) extends Type
final case class Prod(types: Array[Type]) extends Type {
  override def pretty(): String = "(" + types.mkString(",") + ")"
}
final case class Arrow(from: Type, to: Type) extends Type {
  override def pretty(): String = s"(${from.pretty()}) => ${to.pretty()}"
}

object Types {
  val unitT = Prod(Array())
//  val zero = Sum(Array())
//  val alal = zero.to(unit)
}
