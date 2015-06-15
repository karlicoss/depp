package typecheck.inference

import typecheck.Environment.Environment

trait HasEvaluate[T] {
  /**
   * Evaluates the expression under the given context
   * @param env the context
   * @return
   */
  def evaluate(env: Environment): T // TODO transition to Reader?
}
