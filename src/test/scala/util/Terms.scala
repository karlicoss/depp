package util

import terms.Variables._
import terms.{Var, Level, Term}
import typecheck.Environment.Environment
import util.Implicits._

package object Terms {

  /**
   * Returns lambda
   */
  def simpleLambda(v: String, term: Term): Term = v.lam(Level(0), term)

  /**
   * Creates identity function: \x -> x
   */
  def makeId(name: String): Term = simpleLambda(name, name)

  def makeNumeral(n: Int): Term = {
    n match {
      case 0 => "zero"
      case x => "succ" app makeNumeral(x - 1)
    }
  }

  val booleanContext: Environment = Map(
    vv("Bool") -> Level(0),
    vv("true") -> Var("Bool"),
    vv("false") -> Var("Bool")
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
    vv("ff") -> ChurchBoolean.ff
//    vv("not") -> ChurchBoolean.not
  )

  val natContext : Environment = Map(
    vv("Nat") -> Level(0),
    vv("zero") -> Var("Nat"),
    vv("succ") -> ".".pi("Nat", "Nat")
  )
}
