import parser.MyParser
import programs.Programs
import terms.Term

object Interpreter {
  object eval extends MyParser {
    def eval(program: String): Term = {
      val res = parse(program)
      res match {
        case Success(result, next) => {
          val (env, term) = result
          term.evaluateAll(env)
        }
        case _: NoSuccess =>
          throw new RuntimeException()
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val res = eval.eval(Programs.maybe_poly_functor)

    println(res.pretty())
  }
}
