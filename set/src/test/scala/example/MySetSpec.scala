package example

import org.scalatest._

class MySetSpec extends FlatSpec with Matchers {
	"The MySet" should "return some values" in {
		println("Hello")
		val s = MySet(1,2,3,4)
		s + 5 ++ MySet(-1, -2) + 3 flatMap (x => MySet(x, 10 * x)) filter (_ % 2 == 0) foreach println
	}
}
