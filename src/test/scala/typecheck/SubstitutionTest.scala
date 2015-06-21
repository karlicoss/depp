package typecheck

import terms.Variables.{Variable, vv}
import terms.{Lam, Level, App, Var}
import typecheck.Environment.EnvValue
import util.UnitSpec
import util.Implicits._

class SubstitutionTest extends UnitSpec with CustomMatchers {
  it should "perform simple substitution" in {
    val varx: Variable = vv("x'")
    val vary: Variable = vv("y'")
    val term = Var(varx)
    val mapto = App(Var(vary), Var(vary))
    term.subst(Map(varx -> mapto)) should be (App(Var(vary), Var(vary))) // TODO how to use alpha equivalence here?
  }

  it should "a more complex substitution" in {
    val env = Map(vv("0") -> EnvValue(Level(0)))
    val term: Lam = "0".lam("1".lam("0"))
    val res = term.abs.body.subst(Map(term.abs.v -> Var.simple("0")))
    res should beAequivalentTo(env, "y".lam("0"))
  }
}
