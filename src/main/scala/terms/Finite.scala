package terms

import terms.erase.{EFinite, EType, ETerm}
import typecheck.Environment.Environment

import scalaz.State

/**
 * Simple finite type
 * @param elems the elements of the type
 */
final case class Finite(elems: Set[FElem.FElemType]) extends Term {

  override def inferHelper(env: Environment): State[Int, Term] = State.state(Level(0))

  override def substHelper(env: Environment): State[Int, Term] = State.state(this)

  /**
   * Evaluates the expression under the given context
   * @param env the context
   * @return
   */
  override def evaluate(env: Environment): Term = this

  override def pretty(): String = elems.toString()

  override def erase(): Option[Either[ETerm, EType]] = Some(Right(EFinite(null, elems.toList))) // TODO finite type name
}
