package typecheck

import org.scalatest.matchers.{MatchResult, Matcher}
import terms.{Term, Var}
import typecheck.Environment.Environment


trait CustomMatchers {
  class AlphaEquivalenceMatcher(env: Environment, right: Term) extends Matcher[Term] {
    override def apply(left: Term) = MatchResult(
      Alpha.equivalent(env, left, right),
      "Should be alpha-equivalent",
      "Should not be alpha-equivalent")
  }

  class EqualInContextMatcher(env: Environment, right: Term) extends Matcher[Term] {
    override def apply(left: Term) = MatchResult(
      Beta.equivalent(env, left, right),
      s"Expected ${left.pretty()} to be beta-equivalent to ${right.pretty()}",
      "Should not be beta-equivalent")
  }

  class HasTypeInContextMatcher(env: Environment, tp: Term) extends Matcher[Term] {
    override def apply(left: Term) = {
      val inferred = left.infer(env)
      MatchResult(
        Beta.equivalent(env, inferred, tp),
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
    new AlphaEquivalenceMatcher(Map(), right)
  }

  def beAequivalentTo(env: Environment, right: Term): Matcher[Term] = {
    new AlphaEquivalenceMatcher(env, right)
  }

  def beBequivalentTo(env: Environment, right: Term): Matcher[Term] = {
    new EqualInContextMatcher(env, right)
  }

  def haveTypeInContext(env: Environment, tp: Term): Matcher[Term] = {
    new HasTypeInContextMatcher(env, tp)
  }

  val isVar = new VarMatcher
}
