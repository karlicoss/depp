package terms

import terms.Variables.{Dummy, Variable}
import terms.erase.{EType, ETerm}
import typecheck.Environment._
import typecheck.inference.TypeInferenceException

import scalaz.State

final case class TVar(v: Variable) extends Term {
  override def pretty(): String = s"Tv${v.pretty()}"

  override def evaluate(env: Environment): Term = this // TODO??

  override def substHelper(env: Environment) = State.state(this) // TODO ???

  override def inferHelper(env: Environment): State[Int, Term] =
    throw TypeInferenceException("TODO: should this case even be possible?")

  override def erase(): Option[Either[ETerm, EType]] = // TODO eraasure exception?
    throw TypeInferenceException("TODO")
}

object TVar {
  /**
   * TODO: dummy type variables have to be preprocessed
   */
  val dummy = TVar(Dummy())
}
