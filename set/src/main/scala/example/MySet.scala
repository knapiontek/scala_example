
package example

import scala.annotation.tailrec

sealed trait MySet[A] extends (A => Boolean) {
  def apply(elem: A): Boolean = contains(elem)

  def contains(elem: A): Boolean
  def +(elem: A): MySet[A]
  def ++(anotherSet: MySet[A]): MySet[A]
  def map[B](f: A => B): MySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B]
  def filter(predicate: A => Boolean): MySet[A]
  def foreach(f: A => Unit): Unit
}

object MySet {
  def apply[A](elem: A*): MySet[A] = {
    @tailrec
    def build(seq: Seq[A], acc: MySet[A]): MySet[A] = {
      if(seq.isEmpty)
        acc
      else
        build(seq.tail, acc + seq.head)
    }

    build(elem.toSeq, new Sentinel[A])
  }
}
  
final class Sentinel[A] extends MySet[A] {
  def contains(elem: A): Boolean = false
  def +(elem: A): MySet[A] = new NotEmptySet[A](elem, this)
  def ++(other: MySet[A]): MySet[A] = other
  def map[B](f: A => B): MySet[B] = new Sentinel[B]
  def flatMap[B](f: A => MySet[B]): MySet[B] = new Sentinel[B]
  def filter(predicate: A => Boolean): MySet[A] = this
  def foreach(f: A => Unit): Unit = ()
}

class NotEmptySet[A](head: A, tail: MySet[A]) extends MySet[A] {
  override def contains(elem: A): Boolean = {
    if(head == elem) true
    else tail.contains(elem)
  }
  override def +(elem: A): MySet[A] = {
    if(contains(elem)) this
    else new NotEmptySet[A](elem, this)
  }
  override def ++(anotherSet: MySet[A]): MySet[A] = tail ++ anotherSet + head
  override def map[B](f: A => B): MySet[B] = tail.map(f) + f(head)
  override def flatMap[B](f: A => MySet[B]): MySet[B] = f(head) ++ tail.flatMap(f)
  override def filter(predicate: A => Boolean): MySet[A] = {
    val filtered = tail.filter(predicate)
    if(predicate(head)) filtered + head
    else filtered
  }
  override def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }
}
