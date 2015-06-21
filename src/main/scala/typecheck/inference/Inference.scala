package typecheck.inference

import terms.Variables.Variable
import terms._

object Inference {

  /**
   * Substitutes all the occurences of the type variable tvname with the type tp
   */
  def substTv(tvname: Variable, tp: Term, term: Term): Term = {
    def substTvAbs(tvname: Variable, tp: Term, abs: Abs): Abs =
      Abs(abs.v, substTv(tvname, tp, abs.tp), substTv(tvname, tp, abs.body))

    term match {
      case Var(name) => Var(name)
      case TVar(v) => if (v == tvname) tp else TVar(v)
      case Level(kind) => Level(kind)
      case Lam(abs) => Lam(substTvAbs(tvname, tp, abs))
      case Pi(abs) => Pi(substTvAbs(tvname, tp, abs))
      case App(a, b) => App(substTv(tvname, tp, a), substTv(tvname, tp, b))
    }
  }
}
