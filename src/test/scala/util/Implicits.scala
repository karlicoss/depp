package util

import terms.{Term, Var}
import terms.Variables.{Simple, Variable}
import typecheck.Environment.EnvValue

object Implicits {
  implicit def str2Var(name: String): Var = Var.simple(name)

  implicit def str2Variable(name: String): Variable = Simple(name)

  implicit def type2EnvElem(tp: Term): EnvValue = EnvValue(tp)
}
