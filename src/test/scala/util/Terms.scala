package util

import terms.Abstraction.Abs
import terms.Terms.{Var, Level, Lam, Term}
import terms.Variables._

/**
 * Created by karlicos on 30.05.15.
 */
package object Terms {
  def simpleLambda(name: String, term: Term): Term = {
    Lam(Abs(vv(name), Level(0), term))
  }

  def makeId(name: String): Term = {
    simpleLambda(name, Var(vv(name)))
  }
}
