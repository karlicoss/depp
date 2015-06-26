import codegen.Codegen
import codegen.Codegen.indent
import parser.MyParser
import terms.erase.EEnvironment.{TermDecl, EEnvironment, TypeDecl}
import terms.erase._

import scala.collection.mutable

object Main {

  object eval extends MyParser {
    def eval(program: String): String = {
      val res = parse(program)
      res match {
        case Success(result, next) => {
          val (env, term) = result
          val res = term.evaluateAll(env)
          res.pretty()
        }
        case _: NoSuccess =>
          throw new RuntimeException()
      }
    }
  }

  object gen extends Codegen

  def debug(): Unit = {

  }

  def compile(env: EEnvironment, prog: ETerm): mutable.MutableList[String] = {
    gen.generateEnv(env :+ ("res" -> TermDecl(prog)))
    val code: mutable.MutableList[String] = mutable.MutableList()
    code ++= gen.lambdas
    code += "define void @calc() {"
    code ++= indent(gen.code :+ "ret void")
    code += "}"
    code
  }

  def simple(): Unit = {
//    val progs =
//        """
//          | Unit = { uu };
//          | @uu
//        """.stripMargin

    val ee: EEnvironment = List(
      "UU" -> TypeDecl(EFinite("Unit", List("uu"))),
      "ww" -> TermDecl(EFElem("uu", "Unit"))
    )

    val code = compile(ee, EVar("ww"))
    println(code.mkString("\n"))
  }

  def lambda(): Unit = {
    val fin = EFinite("Unit", List("uu"))
//    val lam = ELam("x", fin, ELam("y", fin, ELam("z", fin, EVar("x"))))
//    val lam = ELam("x", fin, ELam("y", fin, EFElem("uu", "Unit")))

//    val cnst = ELam("x", fin, ELam("y", fin, EVar("x")))
//    val prog = EApp(EApp(cnst, EFElem("tt", "Bool")), EFElem("tt", "Bool"))

    val prog = EApp(ELam("x", fin, EVar("x")), EFElem("uu", "Unit"))
    gen.generateAll(Seq("Unit" -> TypeDecl(fin)), prog)

//    val lam = ELam("x", fin, EVar("y"))
//    val env = gen.Closure(Map())
//    val state = gen.GenState(env, Map(), null)
//    gen.generate(lam, state)
    println(gen.datatypes.mkString("\n"))
    println(gen.lambdas.mkString("\n"))
    println("; Code:")
    println(gen.code.mkString("\n"))
  }

  val program =
    """
      | Unit = { uu };
      | id = fun u: Unit. u;
      | id @uu
    """.stripMargin

  val program2 =
    """
      | Unit = { uu };
      | aaa = @uu;
      | aaa
    """.stripMargin

  def main(args: Array[String]): Unit = {
//    simple()
    lambda()
//    println(simple())
//    println(eval.eval(program))
//    val f = EFinite(List("tt", "ff"))
//    println(f.codegen("Bool"))
//    println(Codegen.genProgram(""))
//    println(new MyParser().parse("hey alala @of fl"))
  }
}
