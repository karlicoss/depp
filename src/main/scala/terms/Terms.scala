package terms

import types.Type
import types.Types.unitT

/**
 * Created by karlicos on 30.05.15.
 */

package object Terms {
  type VarName = String

  sealed abstract class Term {
    def pretty(): String
  }

  final case class Var(name: VarName) extends Term {
    override def pretty(): String = name
  }
  final case class Lam(name: VarName, tp: Type, body: Term) extends Term {
    override def pretty(): String = s"\\$name:${tp.pretty()}.${body.pretty()}"
  }
  final case class App(a: Term, b: Term) extends Term {
    override def pretty(): String = s"(${a.pretty()} ${b.pretty()})"
  }
  final case class Prod(terms: Array[Term]) extends Term {
    override def pretty(): String = s"TODO"
  }


  val unit = Prod(Array())
  val ololo = Lam("x", unitT, App(Var("x"), Var("y")))
}


