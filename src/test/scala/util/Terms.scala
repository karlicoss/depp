package util

import terms.Abstraction.Abs
import terms.Terms._
import terms.Variables._
import typecheck.Environment.Environment

/**
 * Created by karlicos on 30.05.15.
 */
package object Terms {
  /**
   * Returns lambda
   */
  def simpleLambda(v: String, term: Term): Term = {
    Lam(Abs(vv(v), Level(0), term))
  }

  def makeId(name: String): Term = {
    simpleLambda(name, Var(vv(name)))
  }

  val booleanContext = Map(
    vv("Bool") -> Level(0),
    vv("true") -> Var(vv("Bool")),
    vv("false") -> Var(vv("Bool"))
  )

  val intContext : Environment = Map(
    vv("Int") -> Level(0),
    vv("zero") -> Var(vv("Int")),
    vv("succ") -> Pi(Abs(vv("."), Var(vv("Int")), Var(vv("Int"))))
  )
}
