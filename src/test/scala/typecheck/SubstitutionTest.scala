package typecheck

import terms.Terms.{App, Var}
import terms.Variables.{Variable, vv}
import typecheck.Substitution.subst
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
    subst(Map(varx -> mapto), term) should be (App(Var(vary), Var(vary))) // TODO how to use alpha equivalence here?
  }
}
