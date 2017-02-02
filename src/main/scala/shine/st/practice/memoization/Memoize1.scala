package shine.st.practice.memoization

/**
  * Created by shinest on 26/01/2017.
  */
class Memoize1[-T, +R](f: T => R) extends (T => R) {

  import scala.collection.mutable

  private[this] val vals = mutable.Map.empty[T, R]

  override def apply(v1: T): R = vals getOrElseUpdate(v1, f(v1))
}

object Memoize {
  def memoize[T, R](f: T => R): (T => R) = new Memoize1(f)

  def memoize[T1, T2, R](f: (T1, T2) => R): (T1, T2) => R = Function.untupled(memoize(f.tupled))

  def memoize[T1, T2, T3, R](f: (T1, T2, T3) => R): (T1, T2, T3) => R = Function.untupled(memoize(f.tupled))

  /**
    * Fixed-point combinator (for memoizing recursive functions).
    */
  def Y[T, R](f: (T => R) => T => R): (T => R) = {
    lazy val yf: (T => R) = memoize(f(yf)(_))
    yf
  }
}