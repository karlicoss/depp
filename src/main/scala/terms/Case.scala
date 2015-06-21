package terms

import typecheck.Beta
import typecheck.Environment.Environment
import typecheck.inference.TypeInferenceException

import scala.collection.Map
import scalaz.State

case class Case(cond: Term, cases: Map[FElem.FElemType, Term], dflt: Option[Term]) extends Term {
  /**
   * Evaluates the expression under the given context
   * @param env the context
   * @return
   */
  override def evaluate(env: Environment): Term = {
    val tev = cond.evaluate(env)
    tev match {
      case FElem(name) => {
        val Finite(elems) = cond.infer(env).evaluateAll(env)
        cases.get(name) match {
          case Some(x) => x
          case None => dflt match {
            case Some(x) => x
            case None => throw TypeInferenceException(s"Expected default case for pattern $name")
          }
        }
      }
      case other =>
        new Case(other, cases, dflt)
    }
  }

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

  def liftOption[S, A](m: Option[State[S, A]]): State[S, Option[A]] = State(s => {
    var curs = s
    var res = null.asInstanceOf[Option[A]]
    m match {
      case Some(x) => {
        val (s, a) = x.apply(curs)
        curs = s
        res = Some(a)
      }
      case None => {
        res = None
      }
    }
    (curs, res)
  })

//  promoteMap(cases.mapValues(_.substHelper(env)))

  override def substHelper(env: Environment): State[Int, Term] = for {
    scond <- cond.substHelper(env)
    scases <- promoteMap(cases.mapValues(_.substHelper(env))) // TODO Applicative.sequence
    sdflt <- liftOption(dflt.map(_.substHelper(env))) // TODO monadic lift?
  } yield new Case(scond, scases, sdflt)

  /**
   * Infers the type of the expression under the given context
   * @param env the context
   * @return
   */
  override def inferHelper(env: Environment): State[Int, Term] = State.state({
    // 1. cond should be Finite
    // 2. check for unknown patterns
    // 3. check that switch is exhaustive
    // 4. types of all the branches should be the same

    val tp = cond.infer(env) // tp should be the name of finite type
    tp match {
      case Var(name) => {
        val Finite(elems) = env.get(name).get.dfn.get // TODO oh my...
        val ccases = cases.keySet
        if (!ccases.subsetOf(elems)) {
          throw TypeInferenceException(s"Unknown cases in $ccases, expected $elems")
        }
        var clauses = cases.values
        if (ccases.size < elems.size) {
          clauses = clauses ++ Seq(dflt.get) // TODO should provide default, throw exception
        }
        val btypes = clauses.map(_.infer(env))
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
  })

  override def pretty(): String = {
    s"case (${cond.pretty()}) of $cases default ${dflt.map(_.pretty())}"
  }
}

object Case {
  def apply(cond: Term, cases: Map[FElem.FElemType, Term]): Case = Case(cond, cases, None)
  def apply(cond: Term, cases: Map[FElem.FElemType, Term], dflt: Term): Case = Case(cond, cases, Some(dflt))
}