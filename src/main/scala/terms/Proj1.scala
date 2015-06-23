package terms

import terms.erase.{EType, EProj1, ETerm}
import typecheck.Environment.Environment

import scala.util.Left
import scalaz.State

case class Proj1(pair: Term) extends Term {

  override def inferHelper(env: Environment): State[Int, Term] = State.state({
    val abs = Common.inferSigma(env, pair)
    abs.tp
  })

  override def substHelper(env: Environment): State[Int, Term] = pair.substHelper(env).map(Proj1)

  /**
   * Evaluates the expression under the given context
   * @param env the context
   * @return
   */
  override def evaluate(env: Environment): Term = {
    val nt = pair.evaluate(env)
    nt match {
      case DPair(a, _, _) => a
      case _ => Proj1(nt)
    }
  }

  override def pretty(): String = toString

  override def erase(): Option[Either[ETerm, EType]] = for {
    pe <- pair.erase()
  } yield Left(EProj1(pe.left.get))
}
