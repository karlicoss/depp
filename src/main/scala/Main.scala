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

  // (\x.\y.x) true false => true
  def runConst(): Unit = {
    val tbool = EFinite("Bool", List("false", "true"))
    val env = Seq("Bool" -> TypeDecl(tbool))

    val cnst = ELam("x", tbool, ELam("y", tbool, EVar("x")))
    val prog = EApp(EApp(cnst, EFElem("true", "Bool")), EFElem("false", "Bool"))

    gen.generateAll(env, prog)
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
    val tbool = EFinite("Bool", List("false", "true")) // bool type declaraton
    val not = ELam("x", tbool, ECase(EVar("x"), Map( // definition of 'not' function
      "true" -> EFElem("false", "Bool"),
      "false" -> EFElem("true", "Bool")), None))
    val env = Seq(
      "Bool" -> TypeDecl(tbool),
      "not"  -> TermDecl(not))
    val prog = EApp(EVar("not"), EFElem("false", "Bool"))

    gen.generateAll(env, prog) // generate actual code

    println()
    println("; Datatypes:")
    println(gen.datatypes.mkString("\n"))
    println()
    println("; Lambdas:")
    println(gen.lambdas.mkString("\n"))
    println()
    println("; Code:")
    println(gen.code.mkString("\n"))
  }
}
