package example

abstract class StreamTrait[+A] {
	def isEmpty: Boolean
	def head: A
	def tail: StreamTrait[A]

	def #::[B >: A](element: B): StreamTrait[B]  // prepend operator
	def ++[B >: A](anotherStream: => StreamTrait[B]): StreamTrait[B] // concatenate two streams

	def foreach(f: A => Unit): Unit
	def map[B](f: A => B): StreamTrait[B]
	def flatMap[B](f: A => StreamTrait[B]): StreamTrait[B]
	def filter(predicate: A => Boolean): StreamTrait[A]

	def take(n: Int): StreamTrait[A] // takes the first n elements out of this stream

	def toList[B >: A](acc: List[B] = Nil): List[B] = {
		if(isEmpty) acc.reverse
		else tail.toList(head :: acc)
	}
}

object StreamSentinel extends StreamTrait[Nothing] {
	def isEmpty: Boolean = true
	def head: Nothing = throw new NoSuchElementException
	def tail: StreamTrait[Nothing] = throw new NoSuchElementException

	def #::[B >: Nothing](element: B): StreamTrait[B] = new TheStream(element, this)
	def ++[B >: Nothing](anotherStream: => StreamTrait[B]): StreamTrait[B] = anotherStream

	def foreach(f: Nothing => Unit): Unit = ()
	def map[B](f: Nothing => B): StreamTrait[B] = this
	def flatMap[B](f: Nothing => StreamTrait[B]): StreamTrait[B] = this
	def filter(predicate: Nothing => Boolean): StreamTrait[Nothing] = this

	def take(n: Int): StreamTrait[Nothing] = this
}

class TheStream[+A](_head: A, _tail: => StreamTrait[A]) extends StreamTrait[A] {
	def isEmpty: Boolean = false
	val head: A = _head
	lazy val tail: StreamTrait[A] = _tail

	def #::[B >: A](element: B): StreamTrait[B] = new TheStream(element, this)
	def ++[B >: A](anotherStream: => StreamTrait[B]): StreamTrait[B] =
		new TheStream[B](head, tail ++ anotherStream)

	def foreach(f: A => Unit): Unit = {
		f(head)
		tail.foreach(f)
	}
	def map[B](f: A => B): StreamTrait[B] = {
		new TheStream[B](f(head), tail.map(f))
	}
	def flatMap[B](f: A => StreamTrait[B]): StreamTrait[B] = f(head) ++ tail.flatMap(f)
	def filter(predicate: A => Boolean): StreamTrait[A] =
		if(predicate(head)) new TheStream(head, tail.filter(predicate))
		else tail.filter(predicate)

	def take(n: Int): StreamTrait[A] = {
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
