package codegen

import terms.erase.EEnvironment.{EEnvironment, TermDecl, TypeDecl}
import terms.erase._

import scala.collection.mutable

class Codegen {
  var cnt = 0

  val lambdas: Map[String, List[String]] = null // TODO
  val finites: Map[String, List[String]] = null // TODO

  val preamble: mutable.MutableList[String] = mutable.MutableList()

  val code: mutable.MutableList[String] = mutable.MutableList()

  def nextVar(): String = {
    val res = s"v$cnt"
    cnt += 1
    res
  }

  def generateEnv(env: EEnvironment): Unit = {

    for ((k, v) <- env) {
      v match {
        case TermDecl(t) => generateTerm(k, t)
        case TypeDecl(t) => generateType(k, t)
        case _ => ???
      }
    }
  }

  def generateTerm(name: String, t: ETerm): Unit = {
    t match {
      case EApp(a, b) => ???
      case EPair(a, b) => ???
      case EVar(v) => ???
      case EFElem(name, fname) => {
        val st = generate(t)
        code ++= st.code
        // TODO store result in name
      }
      case ELam(x, tp, body) => ???
      case EBreak(what, f, s, body) => ???
      case ECase(cond, cases) => ???
      case _ =>
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

  def generate(term: ETerm): St = {
    term match {
      case EPair(a, b) => ???
      case EApp(a, b) => {
        val fcode = generate(a)
        val argcode = generate(b)

        val code = fcode.code ++ argcode.code
        // TODO generate closure?

        // TODO
        St(null, null, null, code) // TODO ??
      }
      case EFElem(name, fname) => {
        val nvar = nextVar()
        val ccode = List(s"%$nvar = load %$fname* @$name")
        // TODO extract EFElem from global scope?
        St(nvar, null, s"%$fname", ccode)
      }
      case EBreak(what, f, s, body) => ???
      case EVar(v) => {
        // TODO extract from closure/extract from lambda
        // TODO the function argument in closure?
        // TODO pass closure in generate function?
        val fresh = nextVar()
        val cltype = null
        val clname = null
        s"$fresh = getelementptr %$cltype* %$clname, i32 0, i32 0"
        // TODO map variable in env to the index
        ???
      }
      case ELam(x, tp, body) => {
        // TODO
        ???
      }
      case ECase(cond, cases) => ???
      case _ => ???
    }
  }

  case class St(res: String, ref: String, tp: String, code: List[String]) {

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
