import parser.MyParser
import programs.Programs

object Interpreter {
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

  def main(args: Array[String]): Unit = {
    eval.eval(Programs.should_be_false)
  }
}
