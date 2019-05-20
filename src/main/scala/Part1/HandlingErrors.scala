package Part1

import scala.{Either => _, Option => _}

//4.1
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this map(Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]


object Option  {
  //4.2
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap(m => mean(xs.map(d => math.pow(d - m,2))))

  //4.3
  def map2_ko[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a,b) match{
    case (None, None) => None
    case (_, None) => None
    case (None, _) => None
    case (Some(a), Some(b)) => Some(f(a,b))
  }
  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f
  def map2[A,B,C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] = oa.flatMap(a => ob.map(b => f(a,b)) )
  def map3[A,B,C,D](oa: Option[A], ob: Option[B], oc: Option[C])(f: (A, B, C) => D): Option[D] = oa.flatMap(a => ob.flatMap(b => oc.map( c => f(a,b,c) ) ) )

  //4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h::t => h.flatMap( hh => sequence(t).map(hh :: _))
  }

  //4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case a::t => f(a).flatMap(oa => traverse(t)(f).map(oa :: _))
  }
  def sequenceWithTraverse[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(identity)



  sealed trait Either[+E, +A]{
    def map[B](f: A => B): Either[E, B] = this match {
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))
    }
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }

    def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(_) => b
      case Right(a) => Right(a)
    }

    def map2[EE >: E, B, C](eb: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this.flatMap(a => eb.map(b => f(a,b)) )
  }
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  def main(args: Array[String]): Unit ={
    println("Some(4).map(x => x*x)", Some(4).map(x => x*x))
    println("Some(4).flatMap(x => Some(x*x))", Some(4).flatMap(x => Some(x*x)))
    println("Some(4).getOrElse(0)", Some(4).getOrElse(0))
    println("None.getOrElse(0)", None.getOrElse(0))
    println("Some(1).orElse(Some(4)", Some(1).orElse(Some(4)))
    println("None.orElse(Some(4)", None.orElse(Some(4)))
    println("Some(4).filter(_=>true)", Some(4).filter(_=>true))
    println("Some(4).filter(_=>false)", Some(4).filter(_=>false))
    println("variance(Seq(4,5,6,10))", variance(Seq(4,5,6,10)))
    println("map2(None, Some(4))((a: Int,b)=> a + b)" , map2(None, Some(4))((a: Int, b)=> a + b) )
    println("map2(Some(6), Some(4))((a,b)=> a + b)" , map2(Some(6), Some(4))((a, b)=> a + b) )
    println("sequence(Some(1)::Some(2)::Some(3)::Nil)" , sequence(Some(1)::Some(2)::Some(3)::Nil) )
    println("sequence(Some(1)::Some(2)::None::Some(3)::Nil)" , sequence(Some(1)::Some(2)::None::Some(3)::Nil) )
    println("traverse(1::2::3::Nil)(i => {println(\"traverse:\" + i);  Some(i*2)})" , traverse(1::2::3::Nil)(i => {println("traverse:" + i);  Some(i*2)}) )
    println("sequenceWithTraverse(Some(1)::Some(2)::Some(3)::Nil)" , sequenceWithTraverse(Some(1)::Some(2)::Some(3)::Nil) )
    println("sequenceWithTraverse(Some(1)::Some(2)::None::Some(3)::Nil)" , sequenceWithTraverse(Some(1)::Some(2)::None::Some(3)::Nil) )
  }
}