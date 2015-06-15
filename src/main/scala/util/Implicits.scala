package util

import terms.Term
import typecheck.Environment.EnvValue

object Implicits {
  implicit def type2EnvElem(tp: Term): EnvValue = EnvValue(tp)
}
