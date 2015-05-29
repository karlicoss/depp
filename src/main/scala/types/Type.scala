package types

/**
 * Created by karlicos on 29.05.15.
 */

sealed abstract class Type {
  def to(other: Type): Arrow = Arrow(this, other)
}

//final case class Sum(types: Array[Type]) extends Type
//final case class Prod(types: Array[Type]) extends Type
final case class Arrow(from: Type, to: Type) extends Type

object Types {
//  val unit = Prod(Array())
//  val zero = Sum(Array())
//  val alal = zero.to(unit)
}
