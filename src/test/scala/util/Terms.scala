package util

import terms.Terms._
import terms.Variables._
import typecheck.Environment.Environment
import util.Implicits._

/**
 * Created by karlicos on 30.05.15.
 */
package object Terms {

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

  object ChurchBoolean {
    val Boolean = "A".pi(Level(0), "t".pi("A", "f".pi("A", "A")))
    val tt = "A".lam(Level(0), "t".lam("A", "f".lam("A", "t")))
    val ff = "A".lam(Level(0), "t".lam("A", "f".lam("A", "f")))
    val not = "b".lam("Boolean",
                "A".lam(Level(0),
                  "t".lam("A",
                    "f".lam("A",
                      "b" app "A" app "f" app "t"))))
  }

  val churchBooleanContext: Environment = Map(
    vv("Boolean") -> ChurchBoolean.Boolean,
    vv("tt") -> ChurchBoolean.tt,
    vv("ff") -> ChurchBoolean.ff,
    vv("not") -> ChurchBoolean.not
  )

  val natContext : Environment = Map(
    vv("Nat") -> Level(0),
    vv("zero") -> "Nat",
    vv("succ") -> ".".pi("Nat", "Nat")
  )
}
