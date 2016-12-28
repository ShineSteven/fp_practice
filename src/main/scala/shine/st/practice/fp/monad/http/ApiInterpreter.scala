package shine.st.practice.fp.monad.http

import cats.~>

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

/**
  * Created by shinest on 23/12/2016.
  */

trait ApiInterpreter {
  type Id[A] = A

  val deafultTimeOut = 5 seconds

  val apiInterp: ~>[ApiAst, Id] = new (ApiAst ~> Id) {
    override def apply[A](fa: ApiAst[A]): A = Await.result(crudProc(fa), deafultTimeOut)
  }

  val futureApiInterp: ~>[ApiAst, Future] = new (ApiAst ~> Future) {
    override def apply[A](fa: ApiAst[A]): Future[A] = crudProc(fa)
  }

  def crudProc[A](fa: ApiAst[A]): Future[A]
}

object ApiInterpreter extends ApiInterpreter {
  override def crudProc[A](fa: ApiAst[A]): Future[A] = fa match {
    case Get(host, port, uri, param) => Future(ApiResponse(200, "Get"))
    case Post(host, port, uri, param, body) => Future(ApiResponse(200, "Post"))
    case Put(host, port, uri, param, body) => Future(ApiResponse(200, "Put"))
    case Delete(host, port, uri, param) => Future(ApiResponse(200, "Delete"))
  }
}