package Part1

import Stream._

sealed trait Stream[+A]{
  //Ex 5.1
  def toList: List[A] = this match{
    case Empty => Nil
    case Cons(h,t) => h()::t().toList
  }

  //Ex 5.2
  def take(n: Int): Stream[A] = (n, this) match{
    case (_, Empty) | (0, _) => Empty
    case (1, Cons(h,_)) => cons(h(), empty)
    case (m, Cons(h,t)) => cons(h(), t().take(m - 1))
  }

  def drop(n: Int): Stream[A] = (n, this) match{
    case (_, Empty) => Empty
    case (0, _) => this
    case (m, Cons(_,t)) => t().drop(m - 1)
  }

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}

object StrictnessAndLaziness  {
  def main(args: Array[String]): Unit ={
    println("cons(4,cons(5,cons(6, empty))).toList ", cons(4,cons(5,cons(6, empty))).toList )
    println("cons(4,cons(5,cons(6, empty))).take(2).toList ", cons(4,cons(5,cons(6, empty))).take(2).toList )
    println("cons(4,cons(5,cons(6, empty))).take(20).toList ", cons(4,cons(5,cons(6, empty))).take(20).toList )
    println("cons(4,cons(5,cons(6, empty))).take(0).toList ", cons(4,cons(5,cons(6, empty))).take(0).toList )
    println("cons(4,cons(5,cons(6, empty))).drop(2).toList ", cons(4,cons(5,cons(6, empty))).drop(2).toList )
    println("cons(4,cons(5,cons(6, empty))).drop(20).toList ", cons(4,cons(5,cons(6, empty))).drop(20).toList )
    println("cons(4,cons(5,cons(6, empty))).drop(0).toList ", cons(4,cons(5,cons(6, empty))).drop(0).toList )
  }
}