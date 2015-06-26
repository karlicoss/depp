package typecheck

import terms.{TVar, Term}
import terms.Variables.Variable

import scala.collection.Map

package object Environment {
  case class EnvValue(tp: Term, dfn: Option[Term]) {
    override def toString: String = tp.pretty() + " : " + dfn.map(_.pretty())
  }

  object EnvValue {
    def apply(tp: Term, dfn: Term): EnvValue = EnvValue(tp, Some(dfn))
    def apply(tp: Term): EnvValue = EnvValue(tp, None)
    def auto(dfn: Term): EnvValue = EnvValue(TVar.dummy, dfn)
  }

  /**
   * Map from variable name to the type and maybe a definition
   */
  type Environment = Map[Variable, EnvValue]
}
