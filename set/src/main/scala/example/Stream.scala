package example

abstract class Stream[+A] {
	def isEmpty: Boolean
	def head: A
	def tail: Stream[A]

	def #::[B >: A](element: B): Stream[B]  // prepend operator
	def ++[B >: A](anotherStream: => Stream[B]): Stream[B] // concatenate two streams

	def foreach(f: A => Unit): Unit
	def map[B](f: A => B): Stream[B]
	def flatMap[B](f: A => Stream[B]): Stream[B]
	def filter(predicate: A => Boolean): Stream[A]

	def take(n: Int): Stream[A] // takes the first n elements out of this stream

	def toList[B >: A](acc: List[B] = Nil): List[B] = {
		if(isEmpty) acc.reverse
		else tail.toList(head :: acc)
	}
}

object StreamSentinel extends Stream[Nothing] {
	def isEmpty: Boolean = true
	def head: Nothing = throw new NoSuchElementException
	def tail: Stream[Nothing] = throw new NoSuchElementException

	def #::[B >: Nothing](element: B): Stream[B] = new TheStream(element, this)
	def ++[B >: Nothing](anotherStream: => Stream[B]): Stream[B] = anotherStream

	def foreach(f: Nothing => Unit): Unit = ()
	def map[B](f: Nothing => B): Stream[B] = this
	def flatMap[B](f: Nothing => Stream[B]): Stream[B] = this
	def filter(predicate: Nothing => Boolean): Stream[Nothing] = this

	def take(n: Int): Stream[Nothing] = this
}

class TheStream[+A](_head: A, _tail: => Stream[A]) extends Stream[A] {
	def isEmpty: Boolean = false
	val head: A = _head
	lazy val tail: Stream[A] = _tail

	def #::[B >: A](element: B): Stream[B] = new TheStream(element, this)
	def ++[B >: A](anotherStream: => Stream[B]): Stream[B] =
		new TheStream[B](head, tail ++ anotherStream)

	def foreach(f: A => Unit): Unit = {
		f(head)
		tail.foreach(f)
	}
	def map[B](f: A => B): Stream[B] = {
		new TheStream[B](f(head), tail.map(f))
	}
	def flatMap[B](f: A => Stream[B]): Stream[B] = f(head) ++ tail.flatMap(f)
	def filter(predicate: A => Boolean): Stream[A] =
		if(predicate(head)) new TheStream(head, tail.filter(predicate))
		else tail.filter(predicate)

	def take(n: Int): Stream[A] = {
		if(n <= 0) StreamSentinel
		else if(n == 1) new TheStream(head, StreamSentinel)
		else new TheStream(head, tail.take(n - 1))
	}
}

object TheStream {
	def from[A](start: A)(generator: A => A): TheStream[A] = {
		new TheStream(start, from(generator(start))(generator))
	}
}

object Playground extends App {
	val naturals = TheStream.from(1)(_ + 1)
	print(naturals.take(10).toList())
}