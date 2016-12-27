package shine.st.practice.fp.monad.http

/**
  * Created by shinest on 23/12/2016.
  */

trait Ast[A]

trait CallAst[A] extends Ast[A] {
  def host: String

  def port: String

  def uri: String

  def param: String
}

case class Get(host: String, port: String, uri: String, param: String) extends CallAst[String]

case class Post(host: String, port: String, uri: String, param: String, body: String) extends CallAst[String]

case class Put(host: String, port: String, uri: String, param: String, body: String) extends CallAst[String]

case class Delete(host: String, port: String, uri: String, param: String) extends CallAst[String]
