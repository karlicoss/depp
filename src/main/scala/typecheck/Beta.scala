package typecheck

import terms.Abstraction.Abs
import terms._
import typecheck.Environment.Environment
import util.Implicits.type2EnvElem

object Beta {
  /**
   * Beta equivalence in context
   */
  def equal(env: Environment, t1: Term, t2: Term): Boolean = {
    def helper(t1: Term, t2: Term): Boolean = {
      (t1, t2) match {
        case (Var(a), Var(b)) => a == b
        case (Level(a), Level(b)) => a == b
        case (Lam(aabs), Lam(babs)) => absHelper(aabs, babs)
        case (Pi(aabs), Pi(babs)) => absHelper(aabs, babs)
        case (App(a, b), App(c, d)) => helper(a, c) && helper(b, d)
        case _ => false // TODO handle type variables?
      }
    }

    def absHelper(a: Abs, b: Abs): Boolean = {
      helper(a.tp, b.tp) && helper(a.body, b.body.subst(Map(b.v -> Var(a.v))))
    }

    helper(t1.evaluate(env), t2.evaluate(env))
  }
}
