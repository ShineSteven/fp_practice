package shine.st.practice.shapeless

import jawn.ast._

trait JsonWrites[-T] {
  def write(t: T): JValue
}

object JsonWrites {
  def apply[T](f: T => JValue) = new JsonWrites[T] {
    def write(t: T) = f(t)
  }
}