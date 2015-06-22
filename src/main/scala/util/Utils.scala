package util

import terms.Finite

object Utils {

  def toFinite(x: Any): Option[Finite] = {
    x match {
      case x1: Finite => Some(x1)
      case _ => None
    }
  }
}
