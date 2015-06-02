package util

import terms.Var
import terms.Variables.{Simple, Variable}

/**
 * Created by karlicos on 31.05.15.
 */
object Implicits {
  implicit def str2Var(name: String): Var = Var.simple(name)

  implicit def str2Variable(name: String): Variable = Simple(name)
}
