package util

import terms.Finite

object Utils {
  def toInstance[T](x: Any): Option[T] = {
    x match {
      case x1: T =>
        Some(x1)
      case _ =>
        None
    }
  }

  def toFinite(x: Any): Option[Finite] = {
    x match {
      case x1: Finite => Some(x1)
      case _ => None
    }
  }
}
