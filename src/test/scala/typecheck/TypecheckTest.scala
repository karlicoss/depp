package typecheck

/**
 * Created by karlicos on 30.05.15.
 */

import org.scalatest._

import scala.collection.mutable.Stack

abstract class UnitSpec extends FlatSpec
  with Matchers with OptionValues with Inside with Inspectors

class TypecheckTest extends UnitSpec {
  "A Stack" should "pop values in last-in-first-out order" in {
    val stack = new Stack[Int]
    stack.push(1)
    stack.push(2)
    stack.pop() should be (2)
    stack.pop() should be (1)
  }

  it should "throw NoSuchElementException if an empty stack is popped" in {
    val emptyStack = new Stack[Int]
    a [NoSuchElementException] should be thrownBy {
      emptyStack.pop()
    }
  }
}