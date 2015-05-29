package terms

import types.Type

/**
 * Created by karlicos on 30.05.15.
 */

package object Terms {
  type VarName = String

  sealed abstract class Term

  final case class Var(name: VarName) extends Term
  final case class Lam(name: VarName, tp: Type, body: Term) extends Term
  final case class App(a: Term, b: Term) extends Term

}


