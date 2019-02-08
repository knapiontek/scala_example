package example

import org.scalatest._

class StreamSpec extends FlatSpec with Matchers {
	"The StreamSpec" should "return generated list of items" in {
		val naturals = TheStream.from(1)(_ + 1)
		val naturalsList = naturals.take(10).toList()
		println(naturalsList)

		val rangeList = List.range(1, 11)
		val zipped = rangeList.zip(naturalsList)
		val diff = zipped.map { case (x, y) => x - y }
		println(diff.sum)
		assert(diff.sum == 0)
	}
}
