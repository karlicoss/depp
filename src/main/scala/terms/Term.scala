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

  /**
   * Constructs a multi-arg application
   * this.app(a1, a2, a3) <=> this a1 a2 a3
   */
  def app(args : Term*): Term = {
    if (args.length == 1) {
      this.app(args.head)
    } else {
      this.app(args.head).app(args.tail:_*)
    }
  }
  
  def ccase(cases: Map[FElem.FElemType, Term]): Case = Case(this, cases)

  def ccase(cases: Map[FElem.FElemType, Term], dflt: Term): Case = Case(this, cases, dflt)

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
