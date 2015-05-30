package types.inference

import terms.Terms._
import types.Type

/**
 * Created by karlicos on 30.05.15.
 */
package object Inference {
  /**
   * Prints out a system of type equations for the given term
   */
  def alala(term: Term): Unit = {
    var v = 0
    def next_id(): String = {
      val res = v
      v += 1
      return s"t$res"
    }
    def getArrow(a: String, b: String): String = s"$a -> $b"

    def printEquation(term: String, hypot: String): Unit = {
      println(s"$term :: $hypot")
    }

    def helper(term: Term): Unit = {
      term match {
        case Var(name) => {
          val id = next_id()
          printEquation(term.pretty(), id)
        }
        case Lam(abs) => {
          val vid = next_id()
          val bid = next_id()
          printEquation(abs.name.pretty(), vid)
          printEquation(abs.body.pretty(), bid)
          printEquation(abs.tp.pretty(), getArrow(vid, bid))
          helper(abs.body)
        }
        case App(a, b) => {
          val appid = next_id()
          val argid = next_id()
          printEquation(a.pretty(), getArrow(argid, appid))
          printEquation(b.pretty(), argid)
          printEquation(term.pretty(), appid)
          helper(a)
          helper(b)
        }
//        case Prod(terms) => {
//          println("TODO")
//        }
      }
    }
    helper(term)
  }
}
