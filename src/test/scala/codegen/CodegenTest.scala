package codegen

import java.io.{PrintWriter, File}

import terms.erase.EEnvironment._
import terms.erase.{ETerm, EFElem, EFinite}
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

  it should "compile" in {
    val env: EEnvironment = List(
      "UU" -> TypeDecl(EFinite("Unit", List("uu")))
    )
    val program = EFElem("uu", "Unit")
    val res = generate(env, program)
    val tmpFile = writeToTmpfile(res.mkString("\n"))
    val lli = Process.apply("lli-3.4", Seq(tmpFile.getAbsolutePath))
    val exitValue = lli.!
    exitValue should be (0)
  }
}
