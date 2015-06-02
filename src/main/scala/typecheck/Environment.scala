package typecheck

import terms.Term
import terms.Variables.Variable

/**
 * Created by karlicos on 30.05.15.
 *
 * TODO: why making it object rather that package object results in
 */
package object Environment {
  type Environment = Map[Variable, Term]
}
