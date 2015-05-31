package util

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
  def simpleLambda(v: String, term: Term): Term = vv(v).lam(Level(0), term)

  def makeId(name: String): Term = simpleLambda(name, Var.simple(name))

  def makeNumeral(n: Int): Term = {
    n match {
      case 0 => Var.simple("zero")
      case x => Var.simple("succ") app makeNumeral(x - 1)
    }
  }

  val booleanContext = Map(
    vv("Bool") -> Level(0),
    vv("true") -> Var.simple("Bool"),
    vv("false") -> Var.simple("Bool")
  )

  val booleanExtendedContext = Map(
//    vv("Boolean") -> Pi(Abs)
  )

  val natContext : Environment = Map(
    vv("Nat") -> Level(0),
    vv("zero") -> Var.simple("Nat"),
    vv("succ") -> vv(".").pi(Var.simple("Nat"), Var.simple("Nat"))
  )
}
