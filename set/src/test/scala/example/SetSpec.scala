package example

import org.scalatest._

class SetSpec extends FlatSpec with Matchers {
	"The TheSet" should "return some values" in {
		println("Hello")
		val s = TheSet(1,2,3,4)
		s + 5 ++ TheSet(-1, -2) + 3 flatMap (x => TheSet(x, 10 * x)) filter (_ % 2 == 0) foreach println
	}
}
