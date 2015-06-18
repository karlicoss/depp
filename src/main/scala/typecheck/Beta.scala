package typecheck

import terms._
import typecheck.Environment.Environment

object Beta {
  /**
   * Beta equivalence in context: evaluation to normal form + alpha
   */
  def equivalent(env: Environment, t1: Term, t2: Term): Boolean =
    Alpha.equivalent(env, t1.evaluateAll(env), t2.evaluateAll(env))
}
