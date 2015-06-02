package terms

import terms.Variables.Variable

/**
 * Created by karlicos on 30.05.15.
 */

object Terms {

  /**
   * let x = y in whatever is equivalent to (\x.whatever) y
   *
   * TODO type should be inferred?
   */
  final class Let(v: Variable, tp: Term, what: Term, dummy: Unit) {
    def in(body: Term): Term = v.lam(tp, body).app(what)
  }

  object Let {
    def apply(v: Variable, tp: Term, what: Term): Let = new Let(v, tp, what, ())
    def apply(v: Variable, what: Term): Let = new Let(v, TVar.dummy, what, ())
  }


}


