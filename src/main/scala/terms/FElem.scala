package terms

import terms.Variables.Variable
import terms.erase.{EFElem, EType, ETerm}
import typecheck.Environment.{EnvValue, Environment}
import typecheck.inference.TypeInferenceException
import util.Utils._

import scalaz.State

/**
 * Finite type element
 */
case class FElem(name: FElem.FElemType) extends Term {

  private def searchFinite(env: Environment): Option[Variable] = {
    def aaa(v: Variable, e: EnvValue): Option[Variable] = for {
      df <- e.dfn
      finite <- toFinite(df)
      if finite.elems.contains(this.name)
    } yield v
    env.toSeq.flatMap(p => aaa(p._1, p._2)).headOption
  }

  override def inferHelper(env: Environment): State[Int, Term] =
    searchFinite(env) match {
      case Some(x) =>
        State.state(Var(x)) // TODO weird..
      case None =>
        throw TypeInferenceException("TODO")
    }

  /**
   * Evaluates the expression under the given context
   * @param env the context
   * @return
   */
  override def evaluate(env: Environment): Term = this

  override def substHelper(env: Environment): State[Int, Term] = State.state(this)

  override def pretty(): String = s"@$name"

  override def erase(): Option[Either[ETerm, EType]] = Some(Left(EFElem(name, null))) // TODO

  override def toString(): String = pretty()
}

object FElem {
  type FElemType = String
}