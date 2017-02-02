package shine.st.practice.memoization

/**
  * Created by shinest on 26/01/2017.
  */
object Main extends App{

  import Memoize._

  def reallySlowFn(i: Int, s: String): Int = {
    Thread.sleep(3000)
    i + s.length
  }

  val memoizedSlowFn = memoize(reallySlowFn _)
  println(memoizedSlowFn(1, "abc")) // returns 4 after about 3 seconds
  println(memoizedSlowFn(1, "abc")) // returns 4 almost instantly


  val fib: BigInt => BigInt = {
    def fibRec(f: BigInt => BigInt)(n: BigInt): BigInt = {
      if (n == 0) 1
      else if (n == 1) 1
      else (f(n-1) + f(n-2))
    }
    Memoize.Y(fibRec)
  }

  println(fib(15))
}
