package typecheck

import org.scalatest.matchers.{MatchResult, Matcher}
import terms.Terms.{Var, Term}
import typecheck.Alpha.equivalent

/**
 * Created by karlicos on 30.05.15.
 */


trait CustomMatchers {
  class AlphaEquivalenceMatcher(right: Term) extends Matcher[Term] {
    override def apply(left: Term) = MatchResult(
      equivalent(left, right),
      "Should be alpha-equivalent",
      "Should not be alpha-equivalent")
  }

  class VarMatcher extends Matcher[Term] {
    override def apply(left: Term): MatchResult = {
      MatchResult(left.isInstanceOf[Var], "NOT INSTANCE!", "INSTANCE!")
    }
  }

  /**
   * TODO how to implement be matcher?
   */
  def beAequivalentTo(right: Term): Matcher[Term] = {
    new AlphaEquivalenceMatcher(right)
  }

  val isVar = new VarMatcher
}
