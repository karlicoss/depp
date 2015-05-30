package typecheck

import terms.Abstraction.Abs
import terms.Terms._
import terms.Variables.Variable

/**
 * Alpha equivalence
 *
 * Created by karlicos on 30.05.15.
 */
package object Alpha {
  def equivalent(a: Term, b: Term): Boolean = {
    def absHelper(ctx: Map[Variable, Variable], a: Abs, b: Abs): Boolean = {
      val typeEquiv = helper(ctx, a.tp, b.tp)
      val bodyEquiv = helper(ctx + (a.name -> b.name), a.body, b.body)
      typeEquiv && bodyEquiv
    }

    def helper(ctx: Map[Variable, Variable], a: Term, b: Term): Boolean = {
      a match {
        case Var(aname) =>
          b match {
            case Var(bname) => ctx(aname) == bname
            case _ => false
          }
        case Lam(aabs) =>
          b match {
            case Lam(babs) => absHelper(ctx, aabs, babs)
            case _ => false
          }
        case App(a1, a2) =>
          b match {
            case App(b1, b2) =>
              helper(ctx, a1, b1) && helper(ctx, a2, b2)
            case _ => false
          }
        case Pi(aabs) =>
          b match {
            case Pi(babs) => absHelper(ctx, aabs, babs)
            case _ => false
          }
        case Universe(akind) =>
          b match {
            case Universe(bkind) => akind == bkind
            case _ => false
          }
      }
    }
    helper(Map(), a, b)
  }
}