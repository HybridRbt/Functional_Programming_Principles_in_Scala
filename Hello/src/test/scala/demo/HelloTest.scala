package demo

import org.scalatest.FunSuite

/**
 * Created by jeredyang on 4/25/14.
 */
class HelloTest extends FunSuite {
  test("sayHello method works correctly. ") {
    val hello = new Hello
    assert(hello.sayHello("Scala") == "Hello, Scala!")

  }
}
