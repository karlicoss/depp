package terms

import typecheck.Environment.Environment

import scalaz.State
import util.Implicits._

case class Proj2(trm: Term) extends Term {
  /**
   * Infers the type of the expression under the given context
   * @param env the context
   * @return
   */
  override def inferHelper(env: Environment): State[Int, Term] = State.state {
    val abs = Common.inferSigma(env, trm)
    val first = Proj1(trm).evaluate(env) // TODO??
    abs.body.subst(Map(abs.v -> first)) // TODO ???
  }

  override def substHelper(env: Environment): State[Int, Term] = for {
    s <- trm.substHelper(env)
  } yield Proj2(s)

  /**
   * Evaluates the expression under the given context
   * @param env the context
   * @return
   */
  override def evaluate(env: Environment): Term = {
    val nt = trm.evaluate(env)
    nt match {
      case DPair(_, b, _) => {
        b
      }
      case _ => Proj2(nt)
    }
  }

  override def pretty(): String = ???
}
