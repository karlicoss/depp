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

  def makeNumeral(n: Int): Term = {
    n match {
      case 0 => Var(vv("zero"))
      case x => App(Var(vv("succ")), makeNumeral(x - 1))
    }
  }

  val booleanContext = Map(
    vv("Bool") -> Level(0),
    vv("true") -> Var(vv("Bool")),
    vv("false") -> Var(vv("Bool"))
  )

  val natContext : Environment = Map(
    vv("Nat") -> Level(0),
    vv("zero") -> Var(vv("Nat")),
    vv("succ") -> Pi(Abs(vv("."), Var(vv("Nat")), Var(vv("Nat"))))
  )
}
