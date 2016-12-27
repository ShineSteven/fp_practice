package shine.st.practice.tlp

import scalaz.Monoid
import scalaz._
import Scalaz._

/**
  * Created by shinest on 25/11/2016.
  */

trait Bar[A] {
  type B

  def value: B
}

object Bar {
  type Aux[A0, B0] = Bar[A0] {type B = B0}

  implicit def bi = new Bar[Int] {
    type B = String

    def value = "bar"
  }

  implicit def bs = new Bar[Boolean] {
    type B = Int

    def value = 33
  }
}

object AuxPattern {
  def main(args: Array[String]): Unit = {
    println(ciao(2))
    println(ciao(true))
  }

  def ciao[T, R](t: T)(implicit f: Bar.Aux[T, R], m: Monoid[R]):R = f.value
}