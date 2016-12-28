package shine.st.practice.fp.monad.http

import shine.st.practice.fp.monad.http.ApiDsl._
import shine.st.practice.fp.monad.http.ApiInterpreter._


/**
  * Created by shinest on 26/12/2016.
  */

object Main {

  case class CombineTest(getResult: String, postResult: String)

  def main(args: Array[String]): Unit = {
    val key = "ip-api"
    val getMonad = get(key, "", Map.empty)

    val getResponse = getMonad.foldMap(apiInterp)
    println(getResponse)

    //    get future, must be import
    import cats.implicits._

    import scala.concurrent.ExecutionContext.Implicits.global

    val getFutureRespose = getMonad.foldMap(futureApiInterp)
    getFutureRespose.onComplete { case r => println(s"complete with $r") }

    val postMonad = post(key, "", Map.empty, "")
    val postResponse = postMonad.foldMap(apiInterp)
    println(postResponse)

    val totalStatusCode = for {
      r1 <- getMonad
      r2 <- postMonad
    } yield r1.statusCode + r2.statusCode

    val totalResponse = totalStatusCode.foldMap(apiInterp)
    println(totalResponse)
  }

}
