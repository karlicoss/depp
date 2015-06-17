package terms

import terms.Finite
import terms.Variables.Variable
import typecheck.Beta
import typecheck.Environment.Environment
import typecheck.inference.TypeInferenceException

import scala.collection.Map
import scalaz.State

case class Case(cond: Term, cases: Map[Variable, Term], dflt: Term) extends Term {
  /**
   * Evaluates the expression under the given context
   * @param env the context
   * @return
   */
  override def evaluate(env: Environment): Term = ???

  /*
    TODO is there a standard library equivalent?
   */
  def promoteMap[K, V, S](m: Map[K, State[S, V]]): State[S, Map[K, V]] = State[S, Map[K, V]](s => {
    var curs = s
    val res = collection.mutable.Map[K, V]()
    for ((k, v) <- m) {
      val (s, a) = v.apply(curs)
      curs = s
      res.put(k, a)
    }
    (curs, res)
  })

//  promoteMap(cases.mapValues(_.substHelper(env)))

  override def substHelper(env: Environment): State[Int, Term] = for {
    scond <- cond.substHelper(env)
    scases <- promoteMap(cases.mapValues(_.substHelper(env))) // TODO Applicative.sequence
    sdflt <- dflt.substHelper(env)
  } yield Case(scond, scases, sdflt)

  /**
   * Infers the type of the expression under the given context
   * @param env the context
   * @return
   */
  override def infer(env: Environment): Term = {
    // 1. cond should be Finite
    // 2. TODO check that switch is exhaustive
    // 3. check for unknown patterns
    // 4. types of all the branches should be the same

    val tp = cond.infer(env) // tp should be the name of finite type
    tp match {
      case Var(name) => {
        val Finite(elems) = env.get(name).get.dfn.get // TODO oh my...
        val ccases = cases.keySet
        if (!ccases.subsetOf(elems)) {
          throw TypeInferenceException(s"Unknown cases in $ccases, expected $elems")
        }
        val btypes = (cases.values ++ Seq(dflt)).map(_.infer(env))
        val ftype = btypes.head
        for (tp <- btypes.tail) {
          if (!Beta.equivalent(env, ftype, tp)) {
            throw TypeInferenceException(s"Expected types $ftype and $tp to be equal!")
          }
        }
        ftype
      }
      case _ =>
        throw TypeInferenceException(s"Expected $tp to be Finite")
    }
  }

  override def pretty(): String = ???
}
