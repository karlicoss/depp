package terms

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
    tp
//    val dv = vv(".")
//    val atp = a.infer(env)
//    val btp = b.infer(env) // TODO what should we add to the environment?
//    // TODO replace a's occurences in btp by dv??
//    Sigma(Abs(dv, atp, btp)) // TODO variable name
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
  override def evaluate(env: Environment): Term = ??? // TODO??? how to evaluate?
}
