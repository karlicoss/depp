package typecheck

import typecheck.Environment.Environment

import scalaz.State

trait HasInference[T] {
  /**
   * Infers the type of the expression under the given context
   * @param env the context
   * @return
   */
  final def infer(env: Environment): T = inferHelper(env).eval(0)

  def inferHelper(env: Environment): State[Int, T] // T is the implicit type variable counter
}
