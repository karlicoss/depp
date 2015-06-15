package typecheck

import terms.Term
import terms.Variables.Variable

package object Environment {
  case class EnvValue(tp: Term, dfn: Option[Term])

  object EnvValue {
    def apply(tp: Term, dfn: Term): EnvValue = EnvValue(tp, Some(dfn))
    def apply(tp: Term): EnvValue = EnvValue(tp, None)
  }

  /**
   * Map from variable name to the type and maybe a definition
   */
  type Environment = Map[Variable, EnvValue]
}
