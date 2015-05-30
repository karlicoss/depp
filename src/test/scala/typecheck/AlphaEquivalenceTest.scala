package typecheck

import terms.Abstraction.Abs
import terms.Terms.{Universe, Var, Lam}
import terms.Variables.vv
import typecheck.Alpha.equivalent
import util.UnitSpec

/**
 * Created by karlicos on 30.05.15.
 */
class AlphaEquivalenceTest extends UnitSpec {
  def makeId(name: String): Lam = {
    val v = vv(name)
    Lam(Abs(v, Universe(0), Var(v)))
  }

  it should "treat as equivalent" in {
    val one = makeId("x")
    val two = makeId("y")
    equivalent(one, two) should be (true)
  }

  it should "treat universes with same indices as eqivalent" in {
    equivalent(Universe(11), Universe(11)) should be (true)
  }

  it should "treat universes with different indices as not equivalent" in {
    equivalent(Universe(1), Universe(2)) should be (false)
  }
}
