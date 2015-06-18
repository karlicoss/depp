package terms

import terms.Abstraction.Abs
import typecheck.Beta
import typecheck.Environment.Environment
import typecheck.inference.TypeInferenceException

import util.Implicits.type2EnvElem

import scalaz.State

case class DPair(a: Term, b: Term, tp: Term) extends Term{
  override def pretty(): String = s"(${a.pretty()} , ${b.pretty()})"

  /**
   * Infers the type of the expression under the given context
   * @param env the context
   * @return
   */
  override def inferHelper(env: Environment): State[Int, Term] = State.state {
    val etp = tp.evaluate(env)
    val Sigma(t) = etp match {
      case Sigma(abs) => etp
      case TVar(v) => {
        val dv = Variables.vv(".")
        val atp = a.infer(env)
        val btp = b.infer(env)
        Sigma(Abs(dv, atp, btp)) // TODO variable name
      }
      case _ => throw TypeInferenceException(s"Sigma type expected, got $etp instead")
    }
    val atp = a.infer(env)
    if (!Beta.equivalent(env, atp, t.tp)) {
      throw TypeInferenceException("TODO")
    }
    val btp = b.infer(env)
    val btype = t.body.subst(Map(t.v -> a))
    if (!Beta.equivalent(env, btp, btype)) {
      throw TypeInferenceException(s"Expected the types $btp and $btype to be b-equivalent")
    }
    Sigma(t)
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
