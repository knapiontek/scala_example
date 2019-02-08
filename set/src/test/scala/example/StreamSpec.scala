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

	it should "generate fibonacci series" in {
		def fibonacci(x: Int, y: Int): TheStream[Int] = {
			new TheStream[Int](x, fibonacci(y, x + y))
		}

		val fibo1 = fibonacci(1, 2).take(10).toList()
		val fibo2 = List(1, 2, 3, 5, 8, 13, 21, 34, 55, 89)

		println(fibo1)
		assert(fibo1 == fibo2)
	}

	it should "generate prime numbers" in {
		def prime(start: Int): TheStream[Int] = {
			new TheStream[Int](start, prime(start + 1).filter(_ % start != 0))
		}

		val prime1 = prime(start=2).take(10).toList()
		val prime2 = List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29)

		println(prime1)
		assert(prime1 == prime2)
	}

	it should "generate fibonacci numbers too" in {
		scala.collection.immutable.Stream
		lazy val fibs: Stream[Int] = 1 #:: 2 #:: fibs.zip(fibs.tail).map { n => n._1 + n._2 }

		val fibo1 = fibs.take(10).toList
		val fibo2 = List(1, 2, 3, 5, 8, 13, 21, 34, 55, 89)

		println(fibo1)
		assert(fibo1 == fibo2)
	}
}
