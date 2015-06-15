package terms

import typecheck.Environment._

import scalaz._

/**
 * Created by karlicos on 03.06.15.
 */
final case class Level(kind: Integer) extends Term {
  override def pretty(): String = s"Type$kind"

  override def evaluate(env: Environment): Term = this

  override def substHelper(env: Environment): State[Int, Term] = State.state(this)

  override def infer(env: Environment): Term = Level(kind + 1)
}