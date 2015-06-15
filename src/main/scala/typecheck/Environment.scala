package typecheck

import terms.Term
import terms.Variables.Variable

import scalaz.Maybe
import scalaz.Maybe.{Empty, Just}

package object Environment {
  case class EnvValue(tp: Term, dfn: Maybe[Term])

  object EnvValue {
    def apply(tp: Term, dfn: Term): EnvValue = EnvValue(tp, Just(dfn))
    def apply(tp: Term): EnvValue = EnvValue(tp, Empty[Term]) // TODO wtf??
  }

  /**
   * Map from variable name to the type and maybe a definition
   */
  type Environment = Map[Variable, EnvValue]
}
