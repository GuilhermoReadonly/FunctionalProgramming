// A comment !
/* Another comment */
/** A documentation comment */
object Test {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def maybeTwice1(b: Boolean, i: => Int) = {
    val j = {
      println("maybeTwice1 1")
      i
    }
    if (b) {
      println("maybeTwice1 2")
      j+j
    } else {
      println("maybeTwice1 3")
      0
    }
  }

  def maybeTwice2(b: Boolean, i: Int) = {
    val j = {
      println("maybeTwice2 1")
      i
    }
    if (b) {
      println("maybeTwice2 2")
      j+j
    } else {
      println("maybeTwice2 3")
      0
    }
  }

  def maybeTwice3(b: Boolean, i: => Int) = {
    lazy val j = {
      println("maybeTwice3 1")
      i
    }
    if (b) {
      println("maybeTwice3 2")
      j + j
    } else {
      println("maybeTwice3 3")
      0
    }
  }


  def main(args: Array[String]): Unit = {
    println(formatAbs(-718))
    maybeTwice1(true, {println("maybeTwice1 0"); 4})
    maybeTwice2(true, {println("maybeTwice2 0"); 4})
    maybeTwice3(true, {println("maybeTwice3 0"); 4})
  }
}