package shine.st.practice.fp.laziness

/**
  * Created by shinest on 2016/9/24.
  */
object StreamMain {
  def main(args: Array[String]): Unit = {
    println(Stream.fibs.take(7).toList)

    val one = Stream.constant(1)
    println(one.take(3).toList)

    val from = Stream.from(5)
    println(from.take(6).toList)



    val stream = Stream(1, 2, 3, 4, 5)
    println(stream.hasSubsequence(Stream(2, 3, 5)))
    println(stream.hasSubsequence2(Stream(2, 3, 4)))
    println

    val stream2 = Stream(1, 2, 3)
    println(stream2.scanRight(0)(_ + _).toList)

  }

}
