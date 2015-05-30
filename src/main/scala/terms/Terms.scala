package terms

import terms.Abstraction.Abs
import terms.Terms.Term
import terms.Variables.Variable
import typecheck.Substitution
import typecheck.inference.Inference
import util.PrettyPrintable

/**
 * Created by karlicos on 30.05.15.
 */



package object Abstraction {

  /**
   * lambda v : tp.body
   */
  final case class Abs(v: Variable, tp: Term, body: Term) extends PrettyPrintable {
    override def pretty(): String = s"\\${v.pretty()}:${tp.pretty()}.${body.pretty()}" // TODO
  }
}

package object Terms {

  sealed abstract class Term extends PrettyPrintable {
    def subst(map: Map[Variable, Term]): Term = {
      Substitution.subst(map, this)
    }
    def equal(other: Term): Boolean = {
      Inference.equal(Map(), this, other)
    }
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
  final case class Level(kind: Integer) extends Term {
    override def pretty(): String = s"Type$kind"
  }


//  val unit = Prod(Array())
//  val ololo = Lam(vv("x"), unitT, App(Var(vv("x")), Var(vv("y"))))
}


