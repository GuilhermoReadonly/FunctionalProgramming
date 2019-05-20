package Part3

import Part1.{None, Option, Some}

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}


object Monoid  {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    val zero = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 * a2
    val zero = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 || a2
    val zero = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 && a2
    val zero = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]) = a1.orElse(a2)
    val zero = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(f: A => A, g: A => A) = f compose g
    val zero = identity
  }

  def main(args: Array[String]): Unit ={
    println("intMultiplication.op(6, 3)", intMultiplication.op(6, 3))
    println("booleanAnd.op(true, false)", booleanAnd.op(true, false))
    println("optionMonoid.op(None, Some(5))", optionMonoid.op(None, Some(5)))
    println("optionMonoid.op(Some(5), None)", optionMonoid.op(Some(5), None))
    println("endoMonoid.op((a: Int)=> a+1, (a: Int)=> a*3))(5)", endoMonoid.op((a: Int)=> a+1, (a: Int)=> a*3)(5) )
    println("endoMonoid.op((a: Int)=> a*3, (a: Int)=> a+1)(5)", endoMonoid.op((a: Int)=> a*3, (a: Int)=> a+1)(5) )
  }
}