package terms

import terms.Abstraction.Abs
import terms.Terms.{TVar, Term}
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
  /*
    TODO: implicit dummy http://stackoverflow.com/a/5828982/706389
   */
  final case class Abs(v: Variable, tp: Term, body: Term, dummy: Unit) extends PrettyPrintable {
    override def pretty(): String = s"${v.pretty()}:${tp.pretty()}.${body.pretty()}"
  }

  object Abs {
    /**
     * Constructs the abstraction; the argument type gets inferred from the type of the expression the abstraction is
     * applied to
     */
    def apply(v: Variable, tp: Term, body: Term): Abs = new Abs(v, tp, body, ())
    /*
      TODO: Should generate a fresh type variable?
      We should probably traverse the term and assign the type variables before the evaluation
     */
    def apply(v: Variable, body: Term): Abs = new Abs(v, TVar(Simple("TODO")), body, ())
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

  final case class TVar(v: Variable) extends Term {
    override def pretty(): String = s"Tv${v.pretty()}"
  }
}


