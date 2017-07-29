package shine.st.practice.shapeless

/**
  * Created by shinest on 27/07/2017.
  */

import jawn.ast._
import shapeless._
import shapeless.labelled.FieldType

object JsonWriteMain {

  implicit val hnilToJson = JsonWrites[HNil](_ => JObject.empty)

  implicit val stringWriter = JsonWrites(JString(_: String))

  implicit val intWriter = JsonWrites[Int](JNum(_))

  implicit val longWriter = JsonWrites[Long](JNum(_))

  implicit def optionWrites[T](implicit tWrites: JsonWrites[T]) = JsonWrites[Option[T]](_.fold[JValue](JNull)(t => tWrites.write(t)))


  // Some nice syntax
  implicit class ToJsonOps[T](t: T)(implicit writes: JsonWrites[T]) {
    def toJson = writes.write(t)
  }


  implicit def hconsToJson[Key <: Symbol, Head, Tail <: HList]
  (
    implicit key: Witness.Aux[Key],
    headWrites: JsonWrites[Head],
    tailWrites: JsonWrites[Tail]
  ): JsonWrites[FieldType[Key, Head] :: Tail] =
    JsonWrites[FieldType[Key, Head] :: Tail] { l =>
      // compute the tail json:
      val json = tailWrites.write(l.tail)
      // JObject has a mutable map, just add to it:
      // (yes, Mutable).
      json.set(key.value.name, headWrites.write(l.head))
      json
    }

  implicit def lgenToJson[T, Repr]
  (
    implicit lgen: LabelledGeneric.Aux[T, Repr],
    reprWrites: JsonWrites[Repr]
  ) = JsonWrites[T] { t =>
    reprWrites.write(lgen.to(t))
  }

  // lgenToJson: [T, Repr](implicit lgen: shapeless.LabelledGeneric.Aux[T,Repr], implicit reprWrites: JsonWrites[Repr])JsonWrites[T]


  def main(args: Array[String]): Unit = {
    val human = Human("Steven", 30)

    println("test".toJson)
    println(LabelledGeneric[Human].to(human).toJson)
    println(human.toJson)

  }


}
