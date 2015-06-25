package codegen

import terms.erase.EEnvironment.{Decl, TermDecl, TypeDecl}
import terms.erase._

import scala.collection.immutable.{Map => IMap}
import scala.collection.mutable.{Map => MMap}
import scala.collection.{Map, mutable}

class Codegen {
  var cnt = 0

  val lambdas: Map[String, List[String]] = null // TODO
  val finites: Map[String, List[String]] = null // TODO

  val preamble: mutable.MutableList[String] = mutable.MutableList()

  val code: mutable.MutableList[String] = mutable.MutableList()

  val curenv: MMap[String, (String, String)] = MMap()

  def indent = (lines: Seq[String]) => lines.map(l => s"  $l")

  def nextVar(): String = {
    val res = s"v$cnt"
    cnt += 1
    res
  }
  
  def nextTmp(): String = {
    val res = s"tmp$cnt"
    cnt += 1
    res
  }

  def nextClosure(): String = {
    val res = s"Cl$cnt"
    cnt += 1
    res
  }

  def generateEnv(env: Seq[(String, Decl)]): Unit = {
    for ((k, v) <- env) {
      v match {
        case TermDecl(t) => {
          generateTerm(k, t)
        }
        case TypeDecl(t) => generateType(k, t)
        case _ => ???
      }
    }
  }

  def generateTerm(name: String, t: ETerm): Unit = {
    t match {
      case EApp(a, b) => ???
      case EPair(a, b) => ???
      case EVar(_) => {
        val st = generate(t, null)
        code ++= st.code
        curenv(name) = (st.res, st.tp)
      }
      case EFElem(_, _) => {
        val st = generate(t, null) // TODO add to env
        code ++= st.code
        curenv(name) = (st.res, st.tp)
        // TODO store result in name
      }
      case ELam(x, tp, body) => ???
      case EBreak(what, f, s, body) => ???
      case ECase(cond, cases) => ???
      case _ =>
    }
  }

  /**
   * Converts the given type to the name of the corresponding structure in LL IR
   */
  def makeIRType(tp: EType): String = {
    tp match {
      case ESum(types) => ???
      case EArrow(left, right) => ???
      case ETuple(a, b) => ???
      case EFinite(fname, elems) => fname
      case _ => ???
    }
  }

  def generateType(name: String, t: EType): Unit = {
    t match {
      case ESum(types) => ???
      case ETuple(a, b) => ???
      case EArrow(left, right) => ???
      case EFinite(fname, elems) =>
        val tp =
          s"""
              |%$fname = type { i32 }
           """.stripMargin
        val values: List[String] = for {
          (elem, i) <- elems.zipWithIndex
        } yield s"@$elem = internal global %$fname { i32 $i }"
        preamble += s"; $fname declaration"
        preamble += tp
        preamble ++= values
        preamble += s"; end of $fname declaration"
      case _ => ???
    }
  }

  /**
   * Environment for lambda functions
   */
  case class Closure(elems: Map[String, EType]) {
    /**
     * Map from variable to its index in LL IR closure
     */
    val index: Map[String, Int]  = elems.keys.zipWithIndex.toMap

    def hasVariable(s: String): Boolean = elems.contains(s)

    /**
     * Creates new closure
     */
    def push(v: String, tp: EType): Closure = Closure(elems + (v -> tp))

    /**
     * Generates IR type definition for the given fields
     */
    def IRtype(types: Iterable[String]): String = "type { " + types.mkString(", ") + " }"

    /**
     * Generates the definition of the LL IR closure
     */
    def generateType(): String = IRtype(elems.values.map(makeIRType))

    /**
     * Generates the code to extract a variable in closure
     * 
     * @param v name of the Depp variable
     */
    def getClosureElement(v: String): St = {
      val tmp = nextTmp()
      val where = nextVar()

      val i = index(v)
      val irType = makeIRType(elems(v))
      val code: Seq[String] = Seq(
        s"%$tmp = getelementptr %$irType*, i32 0, i32 $i",
        s"%$where = load %$irType* %$tmp")
      St(where, null, null, code)
    }
  }

  case class Elem(v: String, tp: EType)

  // priority: first closure, then environment?
  def getType(v: String, state: GenState): EType = {
    if (state.curabs != null && state.curabs.v == v) {
      // TODO
      state.curabs.tp
    } else if (state.closure.hasVariable(v)) {
      // TODO
      ???
    } else {
      state.varenv(v)._2
    }
  }

  def compileLam(lam: ELam, state: GenState): St = {
    // 1. compile the body
    val newState = GenState(state.closure.push(lam.x, lam.tp), state.varenv, Elem(lam.x, lam.tp))
    val cbody = generate(lam.body, newState)
    // TODO initialize the closure
    // TODO add to preamble

    val cltype = nextClosure()

    // 2. create the function
    val rettype = null // TODO
    val argtype = makeIRType(lam.tp)
    val clcode: mutable.MutableList[String] = mutable.MutableList()
    clcode += s"define %$rettype @apply_$cltype(%$cltype* env, %$argtype* ${lam.x}) {"
    clcode ++= indent(cbody.code)
    // clcode += s"ret ????" // TODO
    clcode += "}"
    preamble += "\n"
    preamble ++= clcode
    preamble += "\n"
    St(null, null, null, Seq())
  }

  /**
   * Code generation state
   * @param closure lambda closure
   * @param varenv TODO global variables?
   * @param curabs most recent abstraction
   */
  case class GenState(
                       closure: Closure,
                       varenv: Map[String, (String, EType)],
                       curabs: Elem) {
    /**
     * Just returns the function argument
     */
    def returnBound(): St = {
      val tmp = nextTmp()
      val tp = makeIRType(curabs.tp)
      val code = Seq(s"%$tmp = load $tp* ${curabs.v}")
      St(tmp, null, ???, code)
    }
  }

  def generate(term: ETerm, state: GenState): St = {
    term match {
      case EPair(a, b) => ???
      case EApp(a, b) => {
        val fcode = generate(a, state)
        val argcode = generate(b, state)

        val code = fcode.code ++ argcode.code
        // TODO generate closure?

        // TODO
        St(null, null, null, code) // TODO ??
      }
      case EFElem(name, fname) => {
        val tmp = nextTmp()
        val ccode = Seq(s"%$tmp = load %$fname* @$name")
        // TODO extract EFElem from global scope?
        St(tmp, null, s"%$fname", ccode)
      }
      case EBreak(what, f, s, body) => ???
      case EVar(v) =>
        if (v == state.curabs.v) { // if the variable is the last bound, just return the argument
          state.returnBound()
        } else if (state.closure.hasVariable(v)) { // otherwise, search in closure
          state.closure.getClosureElement(v)
        } else { // otherwise, extract global variable TODO
          ???
        }
      case t@ELam(x, tp, body) => {
        compileLam(t, state)
      }
      case ECase(cond, cases) => ???
      case _ => ???
    }
  }

  def free(t: ETerm): Seq[String] = {
    def helper(t: ETerm, bound: Set[String]): Set[String] = {
      t match {
        case EPair(a, b) => helper(a, bound) ++ helper(b, bound)
        case EApp(a, b) => helper(a, bound) ++ helper(b, bound)
        case EVar(v) => {
          if (bound(v)) Set() else Set(v)
        }
        case EFElem(name, fname) => Set()
        case ELam(x, tp, body) => helper(body, bound + x)
        case EBreak(what, f, s, body) => ??? // TODO
        case ECase(cond, cases) => helper(cond, bound) ++ cases.values.flatMap(x => helper(x, bound))
        case _ => ???
      }
    }
    helper(t, Set()).toSeq
  }

  case class St(res: String, ref: String, tp: String, code: Seq[String]) {

  }
}

object Codegen {
  def genProgram(p: String): String = {
    p +
    """
      | define i32 @main() {
      |   ret i32 0
      | }
    """.stripMargin
  }

//  val program =
//    """
//      | Unit = { uu };
//      | id = fun u: Unit. u;
//      | id @uu
//    """.stripMargin

  //  val program =
  //    """
  //      | Unit = { uu };
  //      | @uu
  //    """.stripMargin
}
