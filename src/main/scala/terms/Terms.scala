package terms

import terms.Variables.Variable
import terms.Variables.vv
import types.Type
import types.Types.unitT
import util.PrettyPrintable

/**
 * Created by karlicos on 30.05.15.
 */

package object Variables {
  sealed abstract class Variable extends PrettyPrintable

  /**
   * The variable entered by the user
   */
  final case class Simple(name: String) extends Variable {
    override def pretty(): String = name
  }

  /**
   * The variable generated while substituting
   * @param name a hint for pretty printing
   */
  final case class Generated(name: String, id: Integer) extends Variable {
    override def pretty(): String = s"$name$id"
  }

  def vv(name: String) = Simple(name)
}

package object Terms {

  sealed abstract class Term extends PrettyPrintable {
  }

  final case class Var(name: Variable) extends Term {
    override def pretty(): String = name.pretty()
  }
  final case class Lam(name: Variable, tp: Type, body: Term) extends Term {
    override def pretty(): String = s"\\${name.pretty()}:${tp.pretty()}.${body.pretty()}"
  }
  final case class App(a: Term, b: Term) extends Term {
    override def pretty(): String = s"(${a.pretty()} ${b.pretty()})"
  }
  final case class Prod(terms: Array[Term]) extends Term {
    override def pretty(): String = s"TODO"
  }


  val unit = Prod(Array())
  val ololo = Lam(vv("x"), unitT, App(Var(vv("x")), Var(vv("y"))))
}


