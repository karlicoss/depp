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

  /**
   * Helper function for type inference.
   * The state is a fresh variable name counter.
   * @param env the context
   * @return
   */
  def inferHelper(env: Environment): State[Int, T]
}
