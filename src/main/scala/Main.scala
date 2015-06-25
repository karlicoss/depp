import codegen.Codegen
import parser.MyParser
import terms.erase.EEnvironment.{TypeDecl, TermDecl, EEnvironment}
import terms.erase.{EFinite, EFElem}

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

  def simple(): Unit = {
//    val progs =
//        """
//          | Unit = { uu };
//          | @uu
//        """.stripMargin

//    val (env, term) = eval.parse(progs).get
    val ee: EEnvironment = Map(
      "UU" -> TypeDecl(EFinite("Unit", List("uu"))),
      "res" -> TermDecl(EFElem("uu", "Unit"))
    )
    gen.generateEnv(ee)

    println(gen.preamble.mkString("\n"))

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
    simple()
//    println(simple())
//    println(eval.eval(program))
//    val f = EFinite(List("tt", "ff"))
//    println(f.codegen("Bool"))
//    println(Codegen.genProgram(""))
//    println(new MyParser().parse("hey alala @of fl"))
  }
}
