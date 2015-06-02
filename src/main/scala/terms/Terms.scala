package terms

import terms.Abstraction.Abs
import terms.Terms.{TVar, Term}
import terms.Variables.{Dummy, Simple, Variable}
import typecheck.Environment.Environment
import typecheck.Substitution
import typecheck.inference.{TypeInferenceException, HasEvaluate, Inference}
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
  final case class Abs(v: Variable, tp: Term, body: Term, dummy: Unit) extends PrettyPrintable with HasEvaluate[Abs] {

    override def pretty(): String = s"${v.pretty()}:${tp.pretty()}.${body.pretty()}"

    override def evaluate(env: Environment): Abs = {
      val etp = tp.evaluate(env)
      val ebody = body.evaluate(env + (this.v -> etp))
      Abs(v, etp, ebody)
    }
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
    def apply(v: Variable, body: Term): Abs = new Abs(v, TVar.dummy, body, ())
  }
}

package object Terms {

  sealed abstract class Term extends PrettyPrintable with HasEvaluate[Term] {
    def subst(map: Environment): Term = Substitution.subst(map, this)

    def equal(other: Term): Boolean = Inference.equal(Map(), this, other)

    def inferType(): Term = Inference.infer(Map(), this)

    def app(other: Term): Term = App(this, other)

    /**
     * Replaces all occurences of dummy type variables with named
     */
    def undummy(): Term = {
      var count = 0

      def absHelper(abs: Abs): Abs = Abs(abs.v, helper(abs.tp), helper(abs.body))

      def helper(term: Term): Term = {
        term match {
          case Var(name) => Var(name)
          case TVar(v) => v match {
            case Dummy() =>
              val res = TVar(Simple(s"tv$count")) // TODO generated?
              count += 1
              res
            case _ => TVar(v)
          }
          case Lam(abs) => Lam(absHelper(abs))
          case Pi(abs) => Pi(absHelper(abs))
          case App(a, b) => App(helper(a), helper(b))
          case Level(kind) => Level(kind)
        }
      }

      helper(this)
    }
  }

  object Var {
    def simple(name: String): Var = Var(Simple(name))
  }

  final case class Var(name: Variable) extends Term {
    override def pretty(): String = name.pretty()

    override def evaluate(env: Environment): Term = {
      env.get(name) match {
        case Some(x) => this
        case None => throw TypeInferenceException(s"Unknown variable ${name.pretty()}") // TODO EvaluationException
      }
    }
  }

  object Lam {
    def create(name: String, tp: Term, body: Term): Lam = Lam(Abs(Simple(name), tp, body))
  }

  final case class Lam(abs: Abs) extends Term {
    override def pretty(): String = "λ" + abs.pretty()

    override def evaluate(env: Environment): Term = Lam(abs.evaluate(env))
  }

  final case class App(a: Term, b: Term) extends Term {
    override def pretty(): String = {
      (a, b) match {
        case (Lam(abs), _) => s"(${a.pretty()}) ${b.pretty()}"
        case (_, Var(v))   => s"${a.pretty()} ${b.pretty()}"
        case _             => s"${a.pretty()} (${b.pretty()})"
      }
    }

    override def evaluate(env: Environment): Term = {
      val arg = b.evaluate(env)
      val fn = a.evaluate(env)
      fn match {
        case Lam(abs) => abs.body.subst(Map(abs.v -> arg)).evaluate(env)
        case _ => App(arg, fn)
      }
    }
  }

  object Pi {
    def create(name: String, tp: Term, body: Term): Pi = Pi(Abs(Simple(name), tp, body))
  }
  final case class Pi(abs: Abs) extends Term {
    override def pretty(): String = "Ɐ" + abs.pretty()

    override def evaluate(env: Environment): Term = Pi(abs.evaluate(env))
  }

  final case class Level(kind: Integer) extends Term {
    override def pretty(): String = s"Type$kind"

    override def evaluate(env: Environment): Term = this
  }

  /**
   * let x = y in whatever is equivalent to (\x.whatever) y
   *
   * TODO type should be inferred?
   */
  final case class Let(v: Variable, tp: Term, what: Term, dummy: Unit) {
    def in(body: Term): Term = v.lam(tp, body).app(what)
  }

  object Let {
    def apply(v: Variable, tp: Term, what: Term): Let = new Let(v, tp, what, ())
    def apply(v: Variable, what: Term): Let = new Let(v, TVar.dummy, what, ())
  }

  final case class TVar(v: Variable) extends Term {
    override def pretty(): String = s"Tv${v.pretty()}"

    override def evaluate(env: Environment): Term = this // TODO??
  }

  object TVar {
    /**
     * TODO: dummy type variables have to be preprocessed
     */
    val dummy = TVar(Dummy())
  }
}


