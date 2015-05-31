package terms

import terms.Abstraction.Abs
import terms.Terms.Term
import terms.Variables.{Simple, Variable}
import typecheck.Environment.Environment
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
    override def pretty(): String = s"${v.pretty()}:${tp.pretty()}.${body.pretty()}" // TODO
  }
}

package object Terms {

  sealed abstract class Term extends PrettyPrintable {
    def subst(map: Environment): Term = {
      Substitution.subst(map, this)
    }

    def equal(other: Term): Boolean = {
      Inference.equal(Map(), this, other)
    }

    def inferType(): Term = {
      Inference.infer(Map(), this)
    }

    def app(other: Term): Term = App(this, other)
  }

  object Var {
    def simple(name: String): Var = Var(Simple(name))
  }

  final case class Var(name: Variable) extends Term {
    override def pretty(): String = name.pretty()
  }

  object Lam {
    def create(name: String, tp: Term, body: Term): Lam = Lam(Abs(Simple(name), tp, body))
  }
  final case class Lam(abs: Abs) extends Term {
    override def pretty(): String = "λ" + abs.pretty()
  }

  final case class App(a: Term, b: Term) extends Term {
    override def pretty(): String = {
      (a, b) match {
        case (Lam(abs), _) => s"(${a.pretty()}) ${b.pretty()}"
        case (_, Var(v))   => s"${a.pretty()} ${b.pretty()}"
        case _             => s"${a.pretty()} (${b.pretty()})"
      }
    }
  }

  object Pi {
    def create(name: String, tp: Term, body: Term): Pi = Pi(Abs(Simple(name), tp, body))
  }
  final case class Pi(abs: Abs) extends Term {
    override def pretty(): String = "Ɐ" + abs.pretty()
  }

  final case class Level(kind: Integer) extends Term {
    override def pretty(): String = s"Type$kind"
  }

  /**
   * let x = y in whatever is equivalent to (\x.whatever) y
   *
   * TODO type should be inferred?
   */
  final case class Let(v: Variable, tp: Term, what: Term) {
    def in(body: Term): Term = v.lam(tp, body).app(what)
  }
}


