package shine.st.practice.tlp

/**
  * Created by shinest on 28/11/2016.
  */

trait Resolver[T, R] {
  def resolve(t: T): R
}

object Resolver {
  implicit val ib: Resolver[Int, Boolean] = new Resolver[Int, Boolean] {
    def resolve(i: Int): Boolean = i > 1
  }
  implicit val sd: Resolver[String, Double] = new Resolver[String, Double] {
    def resolve(i: String): Double = i.length.toDouble
  }
}


object ImplicitResolver {
  def main(args: Array[String]): Unit = {
    val res1: Boolean = foo(3)
    val res2: Double = foo("ciao")
  }

  def foo[T, R](t: T)(implicit p: Resolver[T, R]): R = p.resolve(t)
}
