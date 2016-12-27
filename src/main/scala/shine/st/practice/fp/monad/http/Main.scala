package shine.st.practice.fp.monad.http

import shine.st.practice.fp.monad.http.DslObject._
import shine.st.practice.fp.monad.http.Interpreter._

/**
  * Created by shinest on 26/12/2016.
  */

object Main {
  case class CombineTest(getResult: String, postResult: String)

  def main(args: Array[String]): Unit = {
    val getTest = get("ip-api", "1", "1")
    val result = getTest.foldMap(crudInterp)

    println(result)
    println(test.foldMap(crudInterp))
  }

  def test = for {
    getR <- get("ip-api", "1", "1")
    postR <- post("ip-api", "1", "1", "1")
  } yield CombineTest(getR, postR)
}
