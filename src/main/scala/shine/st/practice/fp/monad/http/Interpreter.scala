package shine.st.practice.fp.monad.http

import cats.~>

/**
  * Created by shinest on 23/12/2016.
  */

trait Interpreter {
  type Id[A] = A

  def crudInterp: ~>[Ast, Id]
}

object Interpreter extends Interpreter {
  def crudInterp = new (Ast ~> Id) {
    override def apply[A](fa: Ast[A]): A = fa match {
      case Get(host, port, uri, param) => "Call Get"
      case Post(host, port, uri, param, body) => "Call Post"
      case Put(host, port, uri, param, body) => "Call Put"
      case Delete(host, port, uri, param) => "Call Delete"
    }
  }
}
