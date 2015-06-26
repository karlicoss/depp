import parser.MyParser
import programs.Programs
import terms.Term
import terms.Variables.vv

object Interpreter {
  object eval extends MyParser {
    def eval(program: String): Term = {
      val res = parse(program)
      res match {
        case Success(result, next) => {
          val (env, term) = result
          val reflType = env.get(vv("reflType")).get.dfn.get
          val res = term.inferAll(env)
          println(reflType.pretty())
          println(res.pretty())
//          val res = Beta.equivalent(env, term.inferAll(env), )
//          println(res)
//          val tp = term.inferAll(env))
          term.evaluateAll(env)
        }
        case _: NoSuccess =>
          throw new RuntimeException()
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val res = eval.eval(Programs.bool_equality)

    println(res.pretty())
  }
}
