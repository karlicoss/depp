package terms

import terms.Variables.Variable
import terms.erase.{EType, ETerm}
import typecheck.Environment.{EnvValue, Environment}
import typecheck.inference.TypeInferenceException

import util.Implicits.type2EnvElem

import scalaz.State

case class Break(what: Term, f: Variable, s: Variable, body: Term) extends Term {
  override def erase(): Option[Either[ETerm, EType]] = ???

  override def substHelper(env: Environment): State[Int, Term] = for { // TODO FIXME GENERATE NEW VARIABLES
    ws <- what.substHelper(env)
    bs <- body.substHelper(env)
  } yield Break(what, f, s, body)

  /**
   * Evaluates the expression under the given context
   * @param env the context
   * @return
   */
  override def evaluate(env: Environment): Term = {
    val ewhat = what.evaluateAll(env)
    ewhat match {
      case v @ Var(name) => {
        Break(v, f, s, body)
      }
      case DPair(a, b, tp) => {
        body.subst(Map(f -> EnvValue(a), s -> EnvValue(b)))
      }
      case _ =>
        throw TypeInferenceException("TODO")
    }
  }

  /**
   * Helper function for type inference.
   * The state is a fresh variable name counter.
   * @param env the context
   * @return
   */
  override def inferHelper(env: Environment): State[Int, Term] = for {
    wt <- what.inferHelper(env).map(_.evaluateAll(env))
    Sigma(abs) = wt // what should be Sigma
    // the type of Break is the type of body
    nenv: Environment = env + (f -> EnvValue(abs.tp)) + (s -> EnvValue(abs.body.app(Var(abs.v))))
    bt <- body.inferHelper(nenv)
  } yield bt

  override def pretty(): String = ???
}
