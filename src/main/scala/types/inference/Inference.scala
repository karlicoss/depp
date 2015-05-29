package types.inference

import terms.Terms._
import types.Type

/**
 * Created by karlicos on 30.05.15.
 */
package object Inference {
  def alala(term: Term): Unit = {
    var v = 0
    def next_id(): Int = {
      val res = v
      v += 1
      res
    }
    def getArrow(a: Int, b: Int): String = s"($a) -> ($b)"

    def helper(term: Term): Unit = {
      term match {
        case Var(name) => {
          val id = next_id()
          println(name + " = " + id)
        }
        case Lam(name, tp, body) => {
          val vid = next_id()
          val bid = next_id()
          println(name + " = " + vid)
          println(body.pretty() + " = " + bid)
          println(term.pretty() + " = " + getArrow(vid, bid))
          helper(body)
        }
        case App(a, b) => {
          val appid = next_id()
          val argid = next_id()
          println(a.pretty() + " = " + getArrow(argid, appid))
          println(b.pretty() + " = " + argid)
          println(term.pretty() + " = " + appid)
          helper(a)
          helper(b)
        }
        case Prod(terms) => {
          println("TODO")
        }
      }
    }
    helper(term)
  }
}
