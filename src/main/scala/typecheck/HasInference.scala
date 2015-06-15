package typecheck

import typecheck.Environment.Environment

trait HasInference[T] {
  /**
   * Infers the type of the expression under the given context
   * @param env the context
   * @return
   */
  def infer(env: Environment): T
}
