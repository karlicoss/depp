package typecheck

import terms.Terms.{App, Var}
import terms.Variables.{Variable, vv}
import util.UnitSpec

/**
 * Created by karlicos on 30.05.15.
 */
class SubstitutionTest extends UnitSpec with CustomMatchers {
  it should "fsf" in {
    val varx: Variable = vv("x'")
    val vary: Variable = vv("y'")
    val term = Var(varx)
    val mapto = App(Var(vary), Var(vary))
    term.subst(Map(varx -> mapto)) should be (App(Var(vary), Var(vary))) // TODO how to use alpha equivalence here?
  }
}
