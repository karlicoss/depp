package terms

import typecheck.Environment.Environment

import scalaz.State

case class Proj1(trm: Term) extends Term {

  /**
   * Infers the type of the expression under the given context
   * @param env the context
   * @return
   */
  override def infer(env: Environment): Term = {
    val abs = Common.inferSigma(env, trm)
    abs.tp
  }

  override def substHelper(env: Environment): State[Int, Term] = for {
    aaa <- trm.substHelper(env)
  } yield Proj1(aaa)

  /**
   * Evaluates the expression under the given context
   * @param env the context
   * @return
   */
  override def evaluate(env: Environment): Term = {
    val nt = trm.evaluate(env)
    nt match {
      case DPair(a, _, _) => a
      case _ => Proj1(nt)
    }
  }

  override def pretty(): String = ???
}
