package terms

import terms.Abstraction.Abs
import terms.Variables.{Variable, Simple, Dummy}
import typecheck.Environment._
import typecheck.{Beta, HasInference}
import typecheck.inference.{HasSubst, HasEvaluate}
import util.PrettyPrintable

import scala.collection.Map
import scala.collection.immutable.{Map => IMap}

// TODO sealed
abstract class Term
  extends PrettyPrintable with HasEvaluate[Term] with HasSubst[Term] with HasInference[Term] {

  /**
   * Beta equality
   */
  def equal(other: Term): Boolean = Beta.equivalent(IMap(), this, other)

  /**
   * Constructs an application
   */
  def app(other: Term): Term = App(this, other)

  def ccase(cases: Map[Variable, Term]): Case = Case(this, cases)

  def ccase(cases: Map[Variable, Term], dflt: Term): Case = Case(this, cases, dflt)

  /**
   * TODO HOW TO MOVE THIS TO HasEvaluate?
   *
   * Repeatedly evaluates the expression under the context until finished
   * @param env the context
   * @return
   */
  def evaluateAll(env: Environment): Term = {
    var cur = this
    var done = false
    while (!done) {
      val res = cur.evaluateAllHelper(env)
      done = res._1
      cur = res._2
    }
    cur
  }
}
