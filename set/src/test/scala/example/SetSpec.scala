package example

import org.scalatest._

class SetSpec extends FlatSpec with Matchers {
	"TheSet" should "return expected values" in {
		println("Hello")
		val s = TheSet(1,2,3,4)
		val s1 = s + 5 ++ TheSet(-1, -2) + 3
		val s2 = s1 flatMap (x => TheSet(x, 10 * x))
		val s3 = s2 filter (_ % 2 == 0)
		println(s3)
	}
}
