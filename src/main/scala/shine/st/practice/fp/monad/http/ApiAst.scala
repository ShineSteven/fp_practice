package shine.st.practice.fp.monad.http

/**
  * Created by shinest on 23/12/2016.
  */

trait ApiAst[A] {
  def host: String

  def port: String

  def uri: String

  def param: Map[String, Any]
}

case class Get(host: String, port: String, uri: String, param: Map[String, Any]) extends ApiAst[ApiResponse]

case class Post(host: String, port: String, uri: String, param: Map[String, Any], body: String) extends ApiAst[ApiResponse]

case class Put(host: String, port: String, uri: String, param: Map[String, Any], body: String) extends ApiAst[ApiResponse]

case class Delete(host: String, port: String, uri: String, param: Map[String, Any]) extends ApiAst[ApiResponse]

case class ApiResponse(statusCode: Int, contentString: String)
