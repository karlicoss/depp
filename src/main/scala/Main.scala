import codegen.Codegen
import parser.MyParser
import terms.erase.EEnvironment.{TermDecl, EEnvironment, TypeDecl}
import terms.erase.{EFElem, EFinite, ETerm}

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

  def compile(env: EEnvironment, prog: ETerm): mutable.MutableList[String] = {
    gen.generateEnv(env :+ ("res" -> TermDecl(prog)))
    val code: mutable.MutableList[String] = mutable.MutableList()
    code ++= gen.preamble
    code += "define void @calc() {"
    code ++= gen.code.map(s => s"  $s")
    code += "ret void"
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
      "UU" -> TypeDecl(EFinite("Unit", List("uu")))
    )

    val code = compile(ee, EFElem("uu", "Unit"))
    println(code.mkString("\n"))
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
    simple()
//    println(simple())
//    println(eval.eval(program))
//    val f = EFinite(List("tt", "ff"))
//    println(f.codegen("Bool"))
//    println(Codegen.genProgram(""))
//    println(new MyParser().parse("hey alala @of fl"))
  }
}
