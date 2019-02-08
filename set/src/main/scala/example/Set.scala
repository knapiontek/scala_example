
package example

import scala.annotation.tailrec

sealed trait TheSet[A] extends (A => Boolean) {
  def apply(elem: A): Boolean = contains(elem)

  def contains(elem: A): Boolean
  def +(elem: A): TheSet[A]
  def -(elem: A): TheSet[A]
  def ++(anotherSet: TheSet[A]): TheSet[A]
  def --(anotherSet: TheSet[A]): TheSet[A]
  def map[B](f: A => B): TheSet[B]
  def flatMap[B](f: A => TheSet[B]): TheSet[B]
  def filter(predicate: A => Boolean): TheSet[A]
  def foreach(f: A => Unit): Unit
  def &(anotherSet: TheSet[A]): TheSet[A]
  def unary_! : TheSet[A]
}

object TheSet {
  def apply[A](elem: A*): TheSet[A] = {
    @tailrec
    def build(seq: Seq[A], acc: TheSet[A]): TheSet[A] = {
      if(seq.isEmpty)
        acc
      else
        build(seq.tail, acc + seq.head)
    }

    build(elem.toSeq, new SetSentinel[A])
  }
}

final class SetSentinel[A] extends TheSet[A] {
  def contains(elem: A): Boolean = false
  def +(elem: A): TheSet[A] = new ElementSet[A](elem, this)
  def -(elem: A): TheSet[A] = this
  def ++(anotherSet: TheSet[A]): TheSet[A] = anotherSet
  def --(anotherSet: TheSet[A]): TheSet[A] = this
  def map[B](f: A => B): TheSet[B] = new SetSentinel[B]
  def flatMap[B](f: A => TheSet[B]): TheSet[B] = new SetSentinel[B]
  def filter(predicate: A => Boolean): TheSet[A] = this
  def foreach(f: A => Unit): Unit = ()
  def &(anotherSet: TheSet[A]): TheSet[A] = this
  def unary_! : TheSet[A] = new PropertySet[A](_ => true)
}

final class ElementSet[A](head: A, tail: TheSet[A]) extends TheSet[A] {
  def contains(elem: A): Boolean = {
    if(head == elem) true
    else tail.contains(elem)
  }
  def +(elem: A): TheSet[A] = {
    if(contains(elem)) this
    else new ElementSet[A](elem, this)
  }
  def -(elem: A): TheSet[A] = {
    if(elem == head) tail
    else tail - elem + head
  }
  def ++(anotherSet: TheSet[A]): TheSet[A] = tail ++ anotherSet + head
  def --(anotherSet: TheSet[A]): TheSet[A] = filter(x => !anotherSet.contains(x))
  def map[B](f: A => B): TheSet[B] = tail.map(f) + f(head)
  def flatMap[B](f: A => TheSet[B]): TheSet[B] = f(head) ++ tail.flatMap(f)
  def filter(predicate: A => Boolean): TheSet[A] = {
    val filtered = tail.filter(predicate)
    if(predicate(head)) filtered + head
    else filtered
  }
  def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }
  def &(anotherSet: TheSet[A]): TheSet[A] = filter(anotherSet) // x => anotherSet.contains(x)
  def unary_! : TheSet[A] = new PropertySet[A](x => !contains(x))
}

final class PropertySet[A](property: A => Boolean) extends TheSet[A] {
  def contains(elem: A): Boolean = property(elem)
  def +(elem: A): TheSet[A] = new PropertySet[A](x => property(x) || x == elem)
  def -(elem: A): TheSet[A] = filter(_ != elem)
  def ++(anotherSet: TheSet[A]): TheSet[A] =
    new PropertySet[A](x => property(x) || anotherSet(x))
  def --(anotherSet: TheSet[A]): TheSet[A] = filter(!anotherSet)
  def map[B](f: A => B): TheSet[B] = fail
  def flatMap[B](f: A => TheSet[B]): TheSet[B] = fail
  def filter(predicate: A => Boolean): TheSet[A] =
    new PropertySet[A](x => property(x) && predicate(x))
  def foreach(f: A => Unit): Unit = fail
  def &(anotherSet: TheSet[A]): TheSet[A] = filter(anotherSet)
  def unary_! : TheSet[A] = new PropertySet[A](x => !property(x))

  def fail = throw new IllegalArgumentException("Infinitive execution time!")
}
