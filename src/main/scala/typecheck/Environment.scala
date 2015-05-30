package typecheck

import terms.Terms.Term
import terms.Variables.Variable

/**
 * Created by karlicos on 30.05.15.
 */
package object Environment {
  type Environment = Map[Variable, Term]

//  def apply(items: (Variable, Term)*): Environment = {
//    ListBuffer(items: _*).toMap
//  }
}
