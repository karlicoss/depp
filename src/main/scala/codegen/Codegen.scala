package codegen

import terms.Variables.Variable
import terms.erase._
import typecheck.Environment.EnvValue

class Codegen {
  var cnt = 0

  val lambdas: Map[String, List[String]]
  val finites: Map[String, List[String]]

  def nextVar(): String = {
    val res = s"v$cnt"
    cnt += 1
    res
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
      }
      case EFElem(name, fname) => {
        // TODO extract EFElem from global scope?
        St(null, s"@$name", s"%$fname", null)
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
      }
      case ELam(x, tp, body) => {
        // TODO
      }
      case ECase(cond, cases) => ???
      case _ =>
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

  def genType(tp: EType): String = ???

//  val program =
//    """
//      | Unit = { uu };
//      | id = fun u: Unit. u;
//      | id @uu
//    """.stripMargin

  def genEnvValue(v: Variable, tp: EType, dfn: ETerm): String = {
    val vname = v.pretty()
    tp match {
      case ESum(types) => ???
      case ETuple(a, b) => ???
      case EArrow(left, right) => ???
      case EFinite(name, elems) =>
      case _ => ???
    }

  }
}
