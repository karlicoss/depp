package terms

import terms.FElem.FElemType
import terms.erase.{ECase, ETerm, EType}
import typecheck.Beta
import typecheck.Environment.EnvValue.auto
import typecheck.Environment.Environment
import typecheck.inference.TypeInferenceException

import scala.collection.Map
import scalaz.State

case class Case(
                 cond: Term,
                 cases: Map[FElem.FElemType, Term],
                 dflt: Option[Term]) extends Term {
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

  def promoteMapOption[K, V](m: Map[K, Option[V]]): Option[Map[K, V]] = {
    val res = collection.mutable.Map[K, V]()
    for ((k, v) <- m) {
      val a = v match {
        case Some(x) => x
        case None => return None
      }
      res.put(k, a)
    }
    Some(res)
  }

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
  override def inferHelper(env: Environment): State[Int, Term] = State.state {
    // 1. cond should be Finite
    // 2. check for unknown patterns
    // 3. check that switch is exhaustive
    // 4. types of all the branches should be the same

    val ctp = cond.infer(env).evaluateAll(env) // tp should be the name of finite type
    val elems = ctp match {
      case Finite(ee) => ee
      case Var(name) => {
        val Finite(ee) = env.get(name).get.dfn.get // TODO oh my...
        ee
      }
      case _ => {
        throw TypeInferenceException(s"Expected $ctp to be Finite")
      }
    }
    val ccases = cases.keySet
    if (!ccases.subsetOf(elems)) {
      throw TypeInferenceException(s"Unknown cases in $ccases, expected $elems")
    }
    var clauses: List[(Term, Environment)] = cond match {
      case Var(name) => {
        // dependent elimination, replace variable with its value
        def eee(f: FElemType): Environment = {
          Map(name -> auto(FElem(f)))
        }
        for {
          (k, v) <- cases.toList
        } yield (v, env ++ eee(k))
      }
      case _ => {
        for {
          (k, v) <- cases.toList
        } yield (v, env)
      }
    }
    if (ccases.size < elems.size) {
      clauses = clauses ++ Seq((dflt.get, env)) // TODO should provide default, throw exception
    }
    val tp = clauses.head._1.inferAll(clauses.head._2)
    for ((cl, ee) <- clauses.tail) {
      val cltp = cl.inferAll(ee)
      if (!Beta.equivalent(ee, tp, cltp)) {
        throw TypeInferenceException(s"Expected types $tp and $cltp to be equal!")
      }
    }
    tp
  }

  override def pretty(): String = {
    s"case (${cond.pretty()}) of $cases default ${dflt.map(_.pretty())}"
  }

  override def erase(): Option[Either[ETerm, EType]] = for {
    econd <- cond.erase()
    emap: Map[FElemType, Either[ETerm, EType]] <- promoteMapOption(cases.mapValues(_.erase()))
//    edflt <- dflt.map(_.erase())
  } yield Left(ECase(econd.left.get, emap.mapValues(_.left.get), null)) // TODO default
}

object Case {
  def apply(cond: Term, cases: Map[FElem.FElemType, Term]): Case = new Case(cond, cases, None) // FIXME
  def apply(cond: Term, cases: Map[FElem.FElemType, Term], dflt: Term): Case = new Case(cond, cases, Some(dflt)) // FIXME
}