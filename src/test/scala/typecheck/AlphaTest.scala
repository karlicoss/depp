package typecheck

import terms.Abstraction.Abs
import terms.Terms.{Universe, Var, Lam}
import terms.Variables.vv
import typecheck.Alpha.equivalent
import util.UnitSpec

/**
 * Created by karlicos on 30.05.15.
 */
class AlphaTest extends UnitSpec {
  def makeId(name: String): Lam = {
    val v = vv(name)
    Lam(Abs(v, Universe(0), Var(v)))
  }

  "fdsf" should "fsdfs" in {
    val one = makeId("x")
    val two = makeId("y")
    equivalent(one, two) should be (true)
  }
}
