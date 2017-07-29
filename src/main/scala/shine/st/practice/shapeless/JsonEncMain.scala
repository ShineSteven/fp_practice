package shine.st.practice.shapeless

/**
  * Created by shinest on 27/07/2017.
  */

import shapeless._
import shapeless.labelled.FieldType
import shine.st.practice.shapeless.Encoder.{ObjEncoder, ValueEncoder}
import record._

object Encoder {

  trait ObjEncoder[-T] {
    def encode(t: T): List[(String, String)]
  }

  trait ValueEncoder[T] {
    def encode(t: T): String
  }

}


object JsonEncMain {

  implicit val emptyEnc: ObjEncoder[HNil] = ((t: HNil) => Nil)
  implicit val intValEnc: ValueEncoder[Int] = ((t: Int) => t.toString)

  implicit val booleanValEnc: ValueEncoder[Boolean] = ((t: Boolean) => t.toString)

  implicit val strValEnc: ValueEncoder[String] = ((t: String) => "\"" + t + "\"")

  implicit def conv[K <: Symbol, H, T <: HList]
  (
    implicit
    witness: Witness.Aux[K],
    hEnc: ValueEncoder[H],
    tEnc: ObjEncoder[T]
  ): ObjEncoder[FieldType[K, H] :: T] = {
    (t: FieldType[K, H] :: T) => {
      (witness.value.name, hEnc.encode(t.head)) +: tEnc.encode(t.tail)
    }
  }

  implicit def getConv[T, R](implicit gen: LabelledGeneric.Aux[T, R], enc: ObjEncoder[R]): ObjEncoder[T] = {
    (t: T) => enc.encode(gen.to(t))
  }

  def hlistToJson[T](h: T)(implicit enc: ObjEncoder[T]): String = {
    enc.encode(h).map { case (key, value) => "\"" + key + "\" :" + value }.mkString("{", ",", "}")
  }

  def main(args: Array[String]): Unit = {
    val human = Human("Steven", 30)

    val fieldHuman = LabelledGeneric[Human].to(human)
    //      fieldHuman:  FieldType["name", String] :: FieldType["age",Int]::HNil
    //  phantom type tagging and literal type


    val name = fieldHuman.get('name)
    println(name)

    println(hlistToJson(LabelledGeneric[Human].to(human)))
    println(hlistToJson(human))
  }


}
