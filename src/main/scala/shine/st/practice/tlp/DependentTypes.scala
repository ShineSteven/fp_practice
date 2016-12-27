package shine.st.practice.tlp

/**
  * Created by shinest on 2016/11/23.
  */
trait Foo {
  type T

  def value: T
}

object FooInt extends Foo {
  type T = Int

  def value: T = 9527
}

object FooString extends Foo {
  type T = String

  def value: T = "Steven"
}


object DependentTypes {
//  Parameter Dependent Types -> foo.T
  def getValue(foo: Foo): foo.T = foo.value

  def main(args: Array[String]): Unit = {
    val fstr = getValue(FooString)
    val fint: Int = getValue(FooInt)
    println(fstr)
    println(fint)
  }
}

