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

  //Ex 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match{
    case Cons(h,t) => if (p(h())) cons(h(), t() takeWhile(p)) else Empty
    case _ => Empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  //Ex 5.4
  def forAll_1(p: A => Boolean): Boolean = this match{
    case Empty => true
    case Cons(h,t) => p(h()) && t().forAll_1(p)
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a,b)=> p(a) && b )

  //Ex 5.5
  def takeWhileFoldRight(p: A => Boolean): Stream[A] = foldRight(empty[A])((h, t) => if(p(h)) cons(h,t) else empty)

  //Ex 5.6
  def headOption: Option[A] = foldRight(None: Option[A])((h, _) => Some(h) )

  //Ex 5.7
  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((h, t) => cons(f(h), t) )
  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((h, t) => f(h) append t )
  def filter(f: A => Boolean): Stream[A] = foldRight(empty[A])((h, t) => if(f(h)) cons(h, t) else t )
  def append[B>:A](s: => Stream[B]): Stream[B] = foldRight(s)((h, t) => cons(h,t) )



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

  //Ex 5.8
  def constant[A](a: A): Stream[A] = {
    lazy val infiniteStream: Stream[A] = Stream.cons(a, infiniteStream)
    infiniteStream
  }

  //Ex 5.9
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  //Ex 5.10
  def fibs(): Stream[Int] = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0+f1))
    go(0, 1)
  }

  //Ex 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty
    case Some((a, s)) => cons(a, unfold(s)(f))
  }
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
    println("cons(4,cons(4,cons(6, empty))).takeWhile( i => i == 4 || i == 5).toList ", cons(4,cons(4,cons(6, empty))).takeWhile( i => i == 4 ).toList )
    println("cons(4,cons(4,cons(6, empty))).forAll( i => i == 4 ) ", cons(4,cons(4,cons(6, empty))).forAll( i => i == 4 ) )
    println("cons(4,cons(4,cons(6, empty))).forAll( i => i%2 == 0 ) ", cons(4,cons(4,cons(6, empty))).forAll( i => i%2 == 0 ) )
    println("cons(4,cons(4,cons(6, empty))).takeWhileFoldRight( i => i == 4 || i == 5).toList ", cons(4,cons(4,cons(6, empty))).takeWhileFoldRight( i => i == 4 ).toList )
    println("cons(4,cons(4,cons(6, empty))).headOption() ", cons(4,cons(4,cons(6, empty))).headOption )
    println("empty.headOption ", empty.headOption )
    println("constant(5) take(6) toList ", constant(5) take(6) toList )
    println("from(5) take(8) toList ", from(5) take(8) toList )
    println("fibs() take(8) toList ", fibs() take(8) toList )
  }
}