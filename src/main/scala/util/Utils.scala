package util

object Utils {
  def toInstance[T](x: Any): Option[T] = x match {
    case x1: T => Some(x1)
    case _ => None
  }
}
