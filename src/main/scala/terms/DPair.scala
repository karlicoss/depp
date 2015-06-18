package terms

import terms.Abstraction.Abs
import typecheck.Beta
import typecheck.Environment.Environment
import typecheck.inference.TypeInferenceException

import util.Implicits.type2EnvElem

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
    val Sigma(t) = tp match {
      case Sigma(abs) => tp
      case TVar(v) => {
        val dv = Variables.vv(".")
        val atp = a.infer(env)
        val btp = b.infer(env)
        Sigma(Abs(dv, atp, btp)) // TODO variable name
      }
      case _ => throw TypeInferenceException(s"Sigma type expected, got $tp instead")
    }
    val atp = a.infer(env)
    if (!Beta.equivalent(env, atp, t.tp)) {
      throw TypeInferenceException("TODO")
    }
    val btp = b.infer(env)
    val btype = t.body.subst(Map(t.v -> a))
    if (!Beta.equivalent(env, btp, btype)) {
      throw TypeInferenceException("TODO")
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
