package shine.st.practice.fp.monad.http

import cats.free.Free
import cats.free.Free.liftF
import com.typesafe.config.ConfigFactory

/**
  * Created by shinest on 23/12/2016.
  */
trait DslObject {
  type Dsl[A] = Free[Ast, A]

  val config = ConfigFactory.load

  protected def get(host: String, port: String, uri: String, param: String): Dsl[String] = liftF(Get(host, port, uri, param))

  protected def post(host: String, port: String, uri: String, param: String, body: String): Dsl[String] = liftF(Post(host, port, uri, param, body))

  protected def put(host: String, port: String, uri: String, param: String, body: String): Dsl[String] = liftF(Put(host, port, uri, param, body))

  protected def delete(host: String, port: String, uri: String, param: String): Dsl[String] = liftF(Delete(host, port, uri, param))

  def get(key: String, uri: String, param: String): Dsl[String] = {
    val host = config.getString(s"services.$key.host")
    val port = config.getString(s"services.$key.port")
    println(s"$host,$port")
    get(host, port, uri, param)
  }

  def post(key: String, uri: String, param: String, body: String): Dsl[String] = {
    val host = config.getString(s"services.$key.host")
    val port = config.getString(s"services.$key.port")
    post(host, port, uri, param, body)
  }

  def put(key: String, uri: String, param: String, body: String): Dsl[String] = {
    val host = config.getString(s"services.$key.host")
    val port = config.getString(s"services.$key.port")
    put(host, port, uri, param, body)
  }

  def delete(key: String, uri: String, param: String): Dsl[String] = {
    val host = config.getString(s"services.$key.host")
    val port = config.getString(s"services.$key.port")
    delete(host, port, uri, param)
  }
}

object DslObject extends DslObject
