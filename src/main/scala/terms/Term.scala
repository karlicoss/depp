package terms

import terms.Variables.Variable
import terms.erase.{EType, ETerm, HasErasure}
import typecheck.Environment._
import typecheck.inference.{HasEvaluate, HasSubst}
import typecheck.{Beta, HasInference}
import util.PrettyPrintable

import scala.collection.Map
import scala.collection.immutable.{Map => IMap}

// TODO sealed
abstract class Term
  extends PrettyPrintable with HasEvaluate[Term] with HasSubst[Term] with HasInference[Term] with HasErasure[Either[ETerm, EType]] {

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

  def break(f: Variable, s: Variable, body: Term): Break = Break(this, f, s, body)

  def ccase(cases: Map[FElem.FElemType, Term]): Case = Case(this, cases)

  def ccase(cases: Map[FElem.FElemType, Term], dflt: Term): Case = Case(this, cases, dflt)

  def ccaset(cases: Map[FElem.FElemType, Term], tp: Term): Case = Case(this, cases, None)

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
//      println("Current: " + cur.pretty())
//      println("Environment: " + env.toList.mkString("\n"))
//      println("-----------------------")
      val res = cur.evaluateAllHelper(env)
      done = res._1
      cur = res._2
    }
    cur
  }

  final def inferAll(env: Environment): Term = infer(env).evaluateAll(env)
}
