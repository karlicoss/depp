package terms

import terms.Abstraction.Abs
import terms.Terms.Term
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

  def vv(name: String): Variable = Simple(name)
}

package object Abstraction {
  final case class Abs(name: Variable, tp: Term, body: Term) extends PrettyPrintable {
    override def pretty(): String = s"\\${name.pretty()}:${tp.pretty()}.${body.pretty()}" // TODO
  }
}

package object Terms {

  sealed abstract class Term extends PrettyPrintable {
  }

  final case class Var(name: Variable) extends Term {
    override def pretty(): String = name.pretty()
  }
  final case class Lam(abs: Abs) extends Term {
    override def pretty(): String = abs.pretty()
  }
  final case class App(a: Term, b: Term) extends Term {
    override def pretty(): String = s"(${a.pretty()} ${b.pretty()})"
  }
  final case class Pi(abs: Abs) extends Term {
    override def pretty(): String = abs.pretty()
  }
  final case class Universe(kind: Integer) extends Term {
    override def pretty(): String = s"Type$kind"
  }


//  val unit = Prod(Array())
//  val ololo = Lam(vv("x"), unitT, App(Var(vv("x")), Var(vv("y"))))
}


