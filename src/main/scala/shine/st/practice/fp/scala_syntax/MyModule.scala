package shine.st.practice.fp.scala_syntax

/**
  * Created by stevenfanchiang on 2016/3/7.
  */
object MyModule {
  def abs(n: Int): Int = {
    if (n < 0)
      -n
    else
      n
  }

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = {
      if (n <= 0)
        acc
      else
        go(n - 1, n * acc)
    }
    go(n, 1)
  }

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d."
    msg.format(n, factorial(n))
  }

  //HOF
  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  // partial apply
  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    (b: B) => f(a, b)


  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(formatFactorial(7))

    val ar = Array(0, 1, 2, 7, 4)
    println(isSorted(ar, (x: Int, y: Int) => y > x))

    findFirst[Int](ar, _ == 7)
  }


  def findFirst[A](ss: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = {
      if (n >= ss.length)
        -1
      else if (p(ss(n)))
        n
      else
        loop(n + 1)
    }
    loop(0)
  }


  //exercise 2.1 page 21
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(add: Int, f0: Int, f1: Int): Int = {
      if (add == n)
        f0
      else
        go(add + 1, f1, f0 + f1)
    }
    go(0, 0, 1)
  }

  //correct answer
  def fib2(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, prev: Int, cur: Int): Int =
      if (n == 0) prev
      else loop(n - 1, cur, prev + cur)
    loop(n, 0, 1)
  }


  //  exercise 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(i: Int): Boolean = {
      if (i < as.size - 1) {
        if (ordered(as(i), as(i + 1)))
          loop(i + 1)
        else
          false
      }
      else
        true
    }
    loop(0)
  }

  //correct answer
  def isSorted2[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(n: Int): Boolean =
      if (n >= as.length - 1) true
      else if (gt(as(n), as(n + 1))) false
      else go(n + 1)

    go(0)
  }

  //exercise 2.3 page 27
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a: A => b: B => f(a, b)
    //        infer type
    //      a => b  => f(a, b)
  }

  //exercise 2.4 page 27
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
    //        infer type
    //      (a, b) => f(a)(b)

  }

  //exercise 2.5 page 27
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a: A => f(g(a))
    //        infer type
    //      a => f(g(a))
  }

  val isSame = new Function2[Int, Int, Boolean] {
    def apply(x: Int, y: Int) = {
      x == y
    }
  }
}
