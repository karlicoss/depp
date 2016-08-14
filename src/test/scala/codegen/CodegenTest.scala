package codegen

import java.io.{PrintWriter, File}

import terms.erase.EEnvironment._
import terms.erase._
import util.UnitSpec

import scala.collection.mutable
import scala.io.Source

import scala.sys.process.ProcessCreation
import scala.sys.process._
import scala.sys.process.Process.cat

class CodegenTest extends UnitSpec{

  def generate(env: EEnvironment, program: ETerm): Seq[String] = {
    object gen extends Codegen
    gen.generateAll(env, program)
    var code = mutable.MutableList[String]()
    code ++= gen.datatypes
    code ++= gen.lambdas
    code ++= gen.code
    code
  }

  def writeToTmpfile(p: String): File = {
    val file = File.createTempFile("depp", null)
    val pw = new PrintWriter(file)
    pw.print(p)
    pw.close()
    file
  }

  /**
   * TODO Use matcher here
   * @param env
   * @param program
   */
  def shouldCompile(env: EEnvironment, program: ETerm): Unit = {
    val res = generate(env, program)
    // TODO how to pipe in an ad-hoc manner instead of creating a temporary file?
    val tmpFile = writeToTmpfile(res.mkString("\n"))
    val lli = Process.apply("lli", Seq(tmpFile.getAbsolutePath))
    val exitValue = lli.!
    exitValue should be (0)
  }

  it should "compile simple program" in {
    val env: EEnvironment = List(
      "UU" -> TypeDecl(EFinite("Unit", List("uu")))
    )
    val program = EFElem("uu", "Unit")
    shouldCompile(env, program)
  }

  it should "compile simple program[2]" in {
    val env: EEnvironment = List(
      "UU" -> TypeDecl(EFinite("Unit", List("uu"))),
      "qq" -> TermDecl(EFElem("uu", "Unit"))
    )
    val program = EVar("qq")
    shouldCompile(env, program)
  }

  it should "compile identity function" in {
    val tunit = EFinite("Unit", List("uu"))
    val env: EEnvironment = List(
      "UU" -> TypeDecl(tunit),
      "id" -> TermDecl(ELam("x", tunit, EVar("x")))
    )
    val program = EApp(EVar("id"), EFElem("uu", "Unit"))
    shouldCompile(env, program)
  }

  it should "compile boolean not function" in {
    val tbool = EFinite("Bool", List("false", "true"))
    val not = ELam("x", tbool, ECase(EVar("x"), Map(
      "true" -> EFElem("false", "Bool"),
      "false" -> EFElem("true", "Bool")), None))
    val env = Seq(
      "Bool" -> TypeDecl(tbool),
      "not"  -> TermDecl(not)
    )
    val program = EApp(EVar("not"), EFElem("false", "Bool"))
    shouldCompile(env, program)
  }

  it should "compile const function" in {
    val tbool = EFinite("Bool", List("false", "true"))
    val const = ELam("x", tbool, ELam("y", tbool, EVar("x")))
    val env = Seq(
      "Bool" -> TypeDecl(tbool),
      "const"  -> TermDecl(const)
    )
    val program = EApp(EApp(EVar("const"), EFElem("true", "Bool")), EFElem("false", "Bool"))
    shouldCompile(env, program)
  }
}
