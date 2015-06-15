package terms

import terms.Abstraction.Abs
import terms.Variables.{Simple, Dummy}
import typecheck.{Beta, HasInference}
import typecheck.inference.{HasSubst, HasEvaluate}
import util.PrettyPrintable

/**
 * Created by karlicos on 03.06.15.
 */
// TODO sealed
abstract class Term
  extends PrettyPrintable with HasEvaluate[Term] with HasSubst[Term] with HasInference[Term] {

  /**
   * Beta equality
   */
  def equal(other: Term): Boolean = Beta.equal(Map(), this, other)

  /**
   * Constructs an application
   */
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
