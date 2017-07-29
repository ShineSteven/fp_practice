package shine.st.practice.shapeless

/**
  * Created by shinest on 27/07/2017.
  */

import shapeless._

trait CsvEncoder[T] {
  def encode(t: T): List[String]
}

object CsvEncMain {
  implicit val nilEnc: CsvEncoder[HNil] = { (t: HNil) => Nil }

  implicit val intEnc: CsvEncoder[Int] = ((t: Int) => List(t.toString))

  implicit val strEnc: CsvEncoder[String] = ((t: String) => List(t.toString))

  implicit val booleanEnc: CsvEncoder[Boolean] = ((t: Boolean) => List(t.toString))

  implicit def conv[H, T <: HList](implicit hEncode: CsvEncoder[H], tEncode: CsvEncoder[T]): CsvEncoder[H :: T] = {
    (t: H :: T) => hEncode.encode(t.head) ++ tEncode.encode(t.tail)
  }

  implicit def getConv[T, R](implicit gen: Generic.Aux[T, R], enc: CsvEncoder[R]): CsvEncoder[T] = {
    (t: T) => enc.encode(gen.to(t))
  }

  //  implicit val stringIntEnc = new CsvEncoder[String :: Int :: HNil] {
  //    override def encode(t: ::[String, ::[Int, HNil]]): List[String] = List(t.head.toString, t.tail.head.toString())
  //  }


  def main(args: Array[String]): Unit = {
    val hl1 = "Steven" :: 30 :: HNil
    println(classToCsv(hl1))

    val human = Human("Steven", 30)
    val humanHl = Generic[Human].to(human)

    println(classToCsv(humanHl))
    println(classToCsv(human))

  }

  def classToCsv[T](h: T)(implicit enc: CsvEncoder[T]): String = {
    enc.encode(h).mkString(",")
  }

}
