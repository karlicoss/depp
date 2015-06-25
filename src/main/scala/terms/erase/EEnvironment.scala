package terms.erase

package object EEnvironment {
  type EEnvironment = Seq[(String, Decl)]

  abstract class Decl

  case class TermDecl(t: ETerm) extends Decl
  case class TypeDecl(t: EType) extends Decl
}
