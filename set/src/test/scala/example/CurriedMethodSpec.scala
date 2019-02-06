package example

import org.scalatest._

class Clazz {

}


// Partially Applied Functions: PAF

class CurriedMethodSpec extends FlatSpec with Matchers {
  "The Hello object" should "say hello" in {
    Hello.greeting shouldEqual "hello"

    val function = (x: Int, y: Int) => x + y

    def method(x: Int, y: Int) = x + y

    def curried(x: Int)(y: Int) = x + y

    val add7_1 = (x: Int) => function(x, 7)
    println(add7_1(1))

    val add7_2 = function.curried(7)
    println(add7_2(1))

    // ETA expansion
    val add7_3 = curried(7) _
    println(add7_3(1))

    val add7_4 = curried(7)(_)
    println(add7_4(1))

    val add7_5 = method(7, _: Int)
    println(add7_5(1))

    def print_me(format: String, value: Double): Unit =
      println(format.format(value))

    print_me("%4.2f", Math.E)

    val numbers = List[Double](Math.E, Math.PI)
    numbers.foreach(print_me("%4.2f", _: Double))
  }

  it should "pass" in {
    def byName(n: => Int) = n + 1
    def byFunction(f: () => Int) = f() + 1

    def method: Int = 42
    def parenMethod(): Int = 42

    // n: => Int - evaluated each time it is used (0 - inf)
    byName(1)
    byName(method)
    byName(parenMethod())
    // byName(parenMethod) // evaluated immediately
    // byName(() => 42)
    // byName(parenMethod(_))

    // byFunction(1)
    // byFunction(method)
    byFunction(parenMethod)
    byFunction(() => 42)
    // byFunction(parenMethod _) // eta-expansion can be done automatically
  }
}
