package typecheck

import org.scalatest.matchers.{MatchResult, Matcher}
import terms.Terms.{Term, Var}
import terms.Variables.Variable
import typecheck.Alpha.equivalent
import typecheck.Environment.Environment

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

  class EqualInContextMatcher(env: Map[Variable, Term], right: Term) extends Matcher[Term] {
    override def apply(left: Term) = MatchResult(
      Beta.equal(env, left, right),
      "Should be beta-equivalent",
      "Should not be beta-equivalent")
  }

  class HasTypeInContextMatcher(env: Environment, tp: Term) extends Matcher[Term] {
    override def apply(left: Term) = {
      val inferred = left.infer(env)
      MatchResult(
        Beta.equal(env, inferred, tp),
        s"Expected type ${tp.pretty()}, got ${inferred.pretty()} instead",
        "Should not have the type"
      )
    }
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

  def beBequivalentTo(env: Environment, right: Term): Matcher[Term] = {
    new EqualInContextMatcher(env, right)
  }

  def haveTypeInContext(env: Environment, tp: Term): Matcher[Term] = {
    new HasTypeInContextMatcher(env, tp)
  }

  val isVar = new VarMatcher
}
