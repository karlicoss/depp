package util

import terms.Terms._
import terms.Variables._
import typecheck.Environment.Environment

/**
 * Created by karlicos on 30.05.15.
 */
package object Terms {

  implicit def str2Var(name: String): Var = Var.simple(name)

  implicit def str2Variable(name: String): Variable = Simple(name)

  /**
   * Returns lambda
   */
  def simpleLambda(v: String, term: Term): Term = v.lam(Level(0), term)

  def makeId(name: String): Term = simpleLambda(name, name)

  def makeNumeral(n: Int): Term = {
    n match {
      case 0 => "zero"
      case x => "succ" app makeNumeral(x - 1)
    }
  }

  val booleanContext: Environment = Map(
    vv("Bool") -> Level(0),
    vv("true") -> "Bool",
    vv("false") -> "Bool"
  )

  val booleanExtendedContext: Environment = Map(
    vv("Boolean") -> "A".pi(Level(0), "t".pi("A", "f".pi("A", "A"))),
    vv("tt") -> "A".lam(Level(0), "t".lam("A", "f".lam("A", "t"))),
    vv("ff") -> "A".lam(Level(0), "t".lam("A", "f".lam("A", "f"))),
    vv("not") -> "b".lam("Boolean",
      "A".lam(Level(0),
        "t".lam("A",
          "f".lam("A",
            "x".app("A").app("f").app("t"))))))

  val natContext : Environment = Map(
    vv("Nat") -> Level(0),
    vv("zero") -> "Nat",
    vv("succ") -> ".".pi("Nat", "Nat")
  )
}
