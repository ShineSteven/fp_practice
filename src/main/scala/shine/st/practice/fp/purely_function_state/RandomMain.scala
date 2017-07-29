package shine.st.practice.fp.purely_function_state

/**
  * Created by shinest on 2016/9/30.
  */
object RandomMain {
  def main(args: Array[String]): Unit = {
    val s = SimpleRNG(5)

    //println(    RNG.intsViaTail(5)(s))
    //    println(RNG.ints2(5)(s))
    //    println(RNG.sequence(List(RNG.unit(1),RNG.unit(2),RNG.unit(3)))(s))
    //    println(RNG.sequence_1(List(RNG.unit(1),RNG.unit(2),RNG.unit(3)))(s))

    val a = RNG.rollDie(s)
    println(a)
    println(RNG.rollDie(a._2))

  }
}
