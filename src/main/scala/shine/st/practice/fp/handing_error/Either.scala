package shine.st.practice.fp.handing_error

/**
  * Created by shinest on 2016/4/24.
  */
sealed trait Either[+E, +A] {

  //EXERCISE 4.6 page 62
  def map[B](f: A => B): Either[E, B] = {
    this match {
      case Right(v) => Right(f(v))
      case Left(e) => Left(e)
    }
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
    this match {
      case Right(v) => f(v)
      case Left(e) => Left(e)
    }
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(e) => b
      case _ => this
    }
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    // for-comprehensions
    for (a <- this; b1 <- b) yield (f(a, b1))
    //flatMap
    //    this flatMap (a => b map (b1 => f(a, b1)))
  }

  //EXERCISE 4.6 end


}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  //  EXERCISE 4.7 page 62
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    traverse(es)(x => x)
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    //    as.foldRight[Either[E, List[B]]](Right(Nil))((x, y) => f(x) flatMap (a => y map (a :: _)))
    as.foldRight[Either[E, List[B]]](Right(Nil))((x, y) => for (a <- f(x); b <- y) yield (a :: b))
    //another answer
    //    as.foldRight[Either[E,List[B]]](Right(Nil))((a, b) => f(a).map2(b)(_ :: _))
  }

  def traverse_1[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es match {
      case Nil => Right(Nil)
      case h :: t => (f(h) map2 traverse_1(t)(f)) (_ :: _)
    }
}