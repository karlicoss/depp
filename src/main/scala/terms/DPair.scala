package terms

import terms.Abstraction.Abs
import typecheck.Environment.Environment

import scalaz.State

case class DPair(a: Term, b: Term, tp: Term) extends Term{
  override def pretty(): String = ??? // TODO

  /**
   * Infers the type of the expression under the given context
   * @param env the context
   * @return
   */
  override def infer(env: Environment): Term = {
    // TODO just return tp for now? When should we typecheck?
    tp match {
      case Sigma(abs) => tp
      case TVar(v) => {
        val dv = Variables.vv(".")
        val atp = a.infer(env)
        val btp = b.infer(env)
        Sigma(Abs(dv, atp, btp)) // TODO variable name
      }
      case _ => ??? // TODO raise exception
    }
  }

  override def substHelper(env: Environment): State[Int, Term] = for {
    aa <- a.substHelper(env)
    bb <- b.substHelper(env)
    tptp <- tp.substHelper(env)
  } yield DPair(aa, bb, tptp)

  /**
   * Evaluates the expression under the given context
   * @param env the context
   * @return
   */
  override def evaluate(env: Environment): Term = {
    val p1 = a.evaluate(env)
    val p2 = b.evaluate(env)
    DPair(p1, p2, tp)
  }
}

object DPair {
  /**
   * Constructs a non-dependent pair
   */
  def apply(a: Term, b: Term): DPair = {
    DPair(a, b, TVar.dummy)
  }

}
