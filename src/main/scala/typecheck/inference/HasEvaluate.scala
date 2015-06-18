package typecheck.inference

import typecheck.Environment.Environment

trait HasEvaluate[T] { self: T =>
  /**
   * Evaluates the expression under the given context
   * @param env the context
   * @return
   */
  def evaluate(env: Environment): T // TODO transition to Reader?

  /**
   * Evaluates the expression under the given context and returns if the evluation is finished
   * @param env
   * @return (true, result) if the evaluation has finished, (false, result) if not finished yet
   */
  def evaluateAllHelper(env: Environment): (Boolean, T) = {
    val ev = self.evaluate(env)
    (self == ev, ev)
  }
}
