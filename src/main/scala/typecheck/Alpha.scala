package typecheck

import terms.Abstraction.Abs
import terms.Variables.Variable
import terms._
import typecheck.Environment.Environment

/**
 * Alpha equivalence
 */
object Alpha {
  def equivalent(env: Environment, a: Term, b: Term): Boolean = {
    def absHelper(env: Environment, map: Map[Variable, Variable], a: Abs, b: Abs): Boolean = {
      val typeEquiv = helper(env, map, a.tp, b.tp)
      val bodyEquiv = helper(env, map + (a.v -> b.v), a.body, b.body)
      typeEquiv && bodyEquiv
    }

    def helper(env: Environment, map: Map[Variable, Variable], a: Term, b: Term): Boolean = {
      a match {
        case Var(aname) =>
          b match {
            case Var(bname) => {
              aname == bname || map(aname) == bname
              // TODO check env.contains(aname) ?
            }
            case _ => false
          }
        case Level(akind) =>
          b match {
            case Level(bkind) => akind == bkind
            case _ => false
          }
        case Lam(aabs) =>
          b match {
            case Lam(babs) => absHelper(env, map, aabs, babs) // TODO variable name into context?
            case _ => false
          }
        case Pi(aabs) =>
          b match {
            case Pi(babs) => absHelper(env, map, aabs, babs)
            case _ => false
          }
        case Sigma(aabs) =>
          b match {
            case Sigma(babs) => absHelper(env, map, aabs, babs)
            case _ => false
          }
        case App(a1, a2) =>
          b match {
            case App(b1, b2) =>
              helper(env, map, a1, b1) && helper(env, map, a2, b2)
            case _ => false
          }
        case Finite(sa) =>
          b match {
            case Finite(sb) => sa.sameElements(sb) // TODO not sure if a good idea
            case _ => false
          }
        case Case(conda, casesa, dflta) =>
          b match {
            case Case(condb, casesb, dfltb) => {
              if (!helper(env, map, conda, condb)) {
                return false
              }
              if (!(casesa.keySet == casesb.keySet)) {
                return false
              }
              for (key <- casesa.keys) {
                if (!helper(env, map, casesa(key), casesb(key))) {
                  return false
                }
              }
              if (dflta.isEmpty ^ dfltb.isEmpty) {
                return false
              }
              if (dflta.isDefined && dfltb.isDefined) {
                if (!helper(env, map, dflta.get, dfltb.get)) {
                  return false
                }
              }
              true
            }
            case _ => false
          }
        // TODO DPair?
      }
    }
    helper(env, Map(), a, b)
  }
}