package shine.st.practice.fp.purely_function_state

/**
  * Created by shinest on 2016/10/3.
  */

//  exercise 6.11 page 90 start
import State._

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object CandyDispenser {

  def update = (input: Input) => (machine: Machine) => (input, machine) match {
    case (_, Machine(_, 0, _)) => machine
    case (Coin, Machine(false, _, _)) => machine
    case (Turn, Machine(true, _, _)) => machine
    case (Coin, Machine(true, candies, coins)) => Machine(false, candies, coins + 1)
    case (Turn, Machine(false, candies, coins)) => Machine(true, candies - 1, coins)
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {

    val step1 = sequence(inputs map (modify[Machine] _ compose update))
    //flatMap 執行了 Machine(true,5,10), map接到了 flatMap 用他的 Machine call 的參數
    step1.flatMap(unitList => get.map { sss => (sss.coins, sss.candies) })

    //    下面是用for的簡潔寫法
    //    for {
    //      _ <- sequence(inputs map (modify[Machine] _ compose update))
    //      s <- get
    //    } yield (s.coins, s.candies)
  }

  def main(args: Array[String]): Unit = {
    val inputs = List(Coin, Turn, Coin, Turn)
    val candy = CandyDispenser.simulateMachine(inputs)
    println(candy.run(Machine(true, 5, 10)))
  }

  //  exercise 6.11 page 90 end
}