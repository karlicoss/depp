package typecheck

import terms.Abstraction.Abs
import terms.Variables.Variable
import terms._

/**
 * Alpha equivalence
 */
object Alpha {
  def equivalent(a: Term, b: Term): Boolean = {
    def absHelper(map: Map[Variable, Variable], a: Abs, b: Abs): Boolean = {
      val typeEquiv = helper(map, a.tp, b.tp)
      val bodyEquiv = helper(map + (a.v -> b.v), a.body, b.body)
      typeEquiv && bodyEquiv
    }

    def helper(map: Map[Variable, Variable], a: Term, b: Term): Boolean = {
      a match {
        case Var(aname) =>
          b match {
            case Var(bname) => map(aname) == bname
            case _ => false
          }
        case Lam(aabs) =>
          b match {
            case Lam(babs) => absHelper(map, aabs, babs)
            case _ => false
          }
        case App(a1, a2) =>
          b match {
            case App(b1, b2) =>
              helper(map, a1, b1) && helper(map, a2, b2)
            case _ => false
          }
        case Pi(aabs) =>
          b match {
            case Pi(babs) => absHelper(map, aabs, babs)
            case _ => false
          }
        case Level(akind) =>
          b match {
            case Level(bkind) => akind == bkind
            case _ => false
          }
      }
    }
    helper(Map(), a, b)
  }
}