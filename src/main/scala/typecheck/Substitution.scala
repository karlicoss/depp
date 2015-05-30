package typecheck

import terms.Abstraction.Abs
import terms.Terms._
import terms.Variables.Variable

/**
 * Created by karlicos on 30.05.15.
 */
package object Substitution {
  // TOOD trait?
  def subst(l: Map[Variable, Term], term: Term): Term = {
    term match {
      case Var(name) =>
        l get name match {
          case Some(x) => x
          case None => Var(name)
        }
      case Lam(abs) => Lam(substAbs(l, abs))
      case Pi(abs) => Pi(substAbs(l, abs))
      case Level(kind) => Level(kind)
      case App(a, b) => App(subst(l, a), subst(l, b))
    }
  }

  def fresh(name: Variable): Variable = ??? // TODO

  def substAbs(l: Map[Variable, Term], abs: Abs): Abs = {
    val fv = fresh(abs.v)
    Abs(fv, subst(l, abs.tp), subst(l + (abs.v -> Var(fv)), abs.body))
  }
}
