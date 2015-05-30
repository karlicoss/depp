package typecheck

/**
 * Created by karlicos on 30.05.15.
 */

import terms.Terms.{App, Var}
import typecheck.Substitution.subst
import types.Types.unitT
import util.UnitSpec
import terms.Variables.{Variable, Simple, vv}


class TypecheckTest extends UnitSpec {
  "fsdf" should "fefewfwe" in {
    val tp = unitT.to(unitT)
//    alala(Terms.ololo)
  }
//  "A Stack" should "pop values in last-in-first-out order" in {
//    val stack = new Stack[Int]
//    stack.push(1)
//    stack.push(2)
//    stack.pop() should be (2)
//    stack.pop() should be (1)
//  }
//
//  it should "throw NoSuchElementException if an empty stack is popped" in {
//    val emptyStack = new Stack[Int]
//    a [NoSuchElementException] should be thrownBy {
//      emptyStack.pop()
//    }
//  }
}

class SubstitutionTest extends UnitSpec {
  "fsfs" should "fsdfsf" in {
    val varx: Variable = vv("x'")
    val vary: Variable = vv("y'")
    val term = Var(varx)
    val mapto = App(Var(vary), Var(vary))
    subst(Map((varx, mapto)), term) should be (App(Var(vary), Var(vary)))// TODO alpha equivalent
  }
}