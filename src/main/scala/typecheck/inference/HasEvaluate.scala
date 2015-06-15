package typecheck.inference

import typecheck.Environment.Environment

/**
 * Created by karlicos on 02.06.15.
 */
trait HasEvaluate[T] {
  /**
   * Evaluates the expression under the given context
   * @param env the context
   * @return
   */
  def evaluate(env: Environment): T // TODO transition to Reader?
}
