package terms

import terms.Variables.{Dummy, Variable}
import typecheck.Environment._
import typecheck.inference.TypeInferenceException

import scalaz.State

/**
 * Created by karlicos on 03.06.15.
 */
final case class TVar(v: Variable) extends Term {
  override def pretty(): String = s"Tv${v.pretty()}"

  override def evaluate(env: Environment): Term = this // TODO??

  override def substHelper(env: Environment) = State.state(this) // TODO ???
  override def infer(env: Environment): Term =
    throw TypeInferenceException("TODO: should this case even be possible?")
}

object TVar {
  /**
   * TODO: dummy type variables have to be preprocessed
   */
  val dummy = TVar(Dummy())
}
