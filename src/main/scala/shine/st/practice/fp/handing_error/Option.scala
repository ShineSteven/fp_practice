package shine.st.practice.fp.handing_error

/**
  * Created by shinest on 2016/4/17.
  */
sealed trait Option[+A] {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty)
      None
    else
      Some(xs.sum / xs.length)

  // EXERCISE 4.1 page 54
  def map[B](f: A => B): Option[B] = {
    this match {
      case Some(get) => Some(f(get))
      case None => None
    }
  }

  def getOrElse[B >: A](default: => B): B = {
    this match {
      case Some(get) => get
      case None => default
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f) getOrElse None
  }

  def flatMap_1[B](f: A => Option[B]): Option[B] = {
    this match {
      case Some(get) => f(get)
      case None => None
    }
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    map(Some(_)) getOrElse ob
  }

  def orElse_1[B >: A](ob: => Option[B]): Option[B] = {
    this match {
      case Some(_) => this
      case _ => ob
    }
  }

  //  current answer
  def filter(f: A => Boolean): Option[A] = {
    flatMap { a => if (f(a))
      Some(a)
    else
      None
    }
  }

  def filter_1(f: A => Boolean): Option[A] = {
    this match {
      case Some(get) if (f(get)) => this
      case _ => None
    }
  }

  //  EXERCISE 4.1 end

  def lift[A, B](f: A => B): Option[A] => Option[B] = x => x map f

  //EXERCISE 4.2 page 55
  //  變異數(variance) = sum((原始值 - 平均值)^2) / length
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))


}

case class Some[+A](get: A) extends Option[A]


case object None extends Option[Nothing]

object Option {
  //EXERCISE 4.3 page 58
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    //    (a, b) match {
    //      case (Some(av), Some(bv)) => Some(f(av, bv))
    //      case (None, Some(_)) => a
    //      case (Some(_), None) => b
    //      case (None, None) => None
    //    }
    //correct answer
    a flatMap (aa => b map (bb => f(aa, bb)))
  }

  //  EXERCISE 4.4 page 59
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    //  a match {
    //    case Nil => None
    //    case (head::tail) => head flatMap {Some(List(_))}
    //  }
    //correct answer
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }
  }

  def sequence_1[A](a: List[Option[A]]): Option[List[A]] = {
    //    a.foldRight[Option[List[A]]](Some(Nil)) { (h, b) => h flatMap { hh => b map (hh :: _) }
    //    graceful answer
    a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)(_ :: _))
  }

  //EXERCISE 4.5 page 59
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a match {
      case Nil => Some(Nil)
      case h :: t => f(h) flatMap (hh => traverse(t)(f) map (hh :: _))
    }
  }

  def traverse_1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight[Option[List[B]]](Some(Nil)) { (x, y) => map2(f(x), y)(_ :: _) }
  }

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a)(x=>x)
  }

}