package shine.st.practice.fp.laziness


/**
  * Created by shinest on 2016/9/20.
  */
trait Stream[+A] {

  import Stream._

  //exercise 5.1 page 69 start
  //my answer = toListRecursive // The natural recursive solution, 用stack frame的方式做處理，not tail-recursive
  def toListRecursive(): List[A] = {
    this match {
      case Empty => List.empty[A]
      case Cons(h, t) => h() :: t().toListRecursive()
    }
  }

  def toList: List[A] = {
    @annotation.tailrec
    def go(stream: Stream[A], acc: List[A]): List[A] = {
      stream match {
        case Cons(h, t) => go(t(), h() :: acc)
        case _ => acc
      }
    }
    go(this, List()).reverse
  }


  /*
In order to avoid the `reverse` at the end, we could write it using a
mutable list buffer and an explicit loop instead. Note that the mutable
list buffer never escapes our `toList` method, so this function is
still _pure_.
*/
  def toListFast: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h, t) =>
        buf += h()
        go(t())
      case _ => buf.toList
    }
    go(this)
  }

  //exercise 5.1 page 69 end

  //  exercise 5.2 page 70 start
  //  my answer: 直接用Cons會讓programmer難以維護，因為每次的stream都沒做完
  //    def take(n: Int): Stream[A] = {
  //      println(n)
  //      this match {
  //        case Empty => Empty
  //        case Cons(h, t) => Cons(h, () => t().take(n - 1))
  //      }
  //    }


  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, t) if n == 1 => cons(h(), empty)
    case _ => empty
  }


  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  //  exercise 5.2 page 70 end

  //  exercise 5.3 page 70 start
  //  same to my take description

  //  def takeWhile2(b: A => Boolean): Stream[A] = this match {
  //    case Empty => Empty
  //    case Cons(h, t) if b(h()) => Cons(h, () => t().takeWhile(b))
  //  }


  def takeWhile(b: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if (b(h())) => cons(h(), t().takeWhile(b))
    case _ => empty
  }

  //  exercise 5.3 page 70 end

  //  high order function f take its seconds  argument by name parameter, => B 不一定會被做到，看f裡的定義
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }


  def exists(p: A => Boolean): Boolean = {
    this match {
      case Cons(h, t) => p(h()) || t().exists(p)
      case _ => false
    }
  }

  def existsViaFoldRight(p: A => Boolean): Boolean = {
    foldRight(false)((a, b) => p(a) || b)
  }

  //  exercise 5.4 page 71 start
  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a, b) => p(a) && b)
  }

  //  exercise 5.4 page 71 end

  //  exercise 5.5 page 71 start
  //  wrong answer
  //  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = {
  //    foldRight[Stream[A]](this)((a, b) => if (p(a))
  //      cons(a, b)
  //    else
  //      cons(a,Empty)
  //
  //    )
  //  }

  def takeWhileViaFoldRight(f: A => Boolean): Stream[A] =
    foldRight(empty[A]) { (h, t) =>
      if (f(h))
        cons(h, t)
      else
        empty
    }

  //  exercise 5.5 page 71 end

  //  exercise 5.6 page 71 start
  def headOption() = {
    foldRight[Option[A]](None)((h, t) => Some(h))
  }

  //  answer 寫法
  def headOption2() = {
    foldRight(None: Option[A])((h, t) => Some(h))
  }

  //  exercise 5.6 page 71 end

  //  exercise 5.7 page 72 start

  def map[B](f: A => B): Stream[B] = {
    foldRight(empty[B])((h, t) => cons(f(h), t))
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight(empty[A])((h, t) => if (f(h))
      cons(h, t)
    else
      t
    )
  }

  //  wrong
  //  def append(e:  A): Stream[A] = {
  //    foldRight(cons(e, empty))((h, t) => cons(h, t))
  //  }

  def append[B >: A](e: => Stream[B]): Stream[B] = {
    foldRight(e)((h, t) => cons(h, t))
  }


  //  wrong
  //  def flatMap[B](f: => A => B): Stream[B] = {
  //    foldRight(empty[B])((h, t) => cons(f(h), t))
  //  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])((h, t) => f(h) append t)
  }

  //  exercise 5.7 page 72 end

  def find(f: A => Boolean): Option[A] = {
    filter(f).headOption()
  }


  //  exercise 5.13 page 76 start
  def mapViaUnfold[B](f: A => B): Stream[B] = {
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }
  }

  def takeViaUnfold(n: Int): Stream[A] = {
    unfold((this, n)) {
      case (Cons(h, t), 1) => Some(h(), (empty, 0))
      case (Cons(h, t), e) if e > 1 => Some(h(), (t(), e - 1))
      case _ => None
    }
  }

  def takeWhileViaUnfold(f: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if f(h()) => Some(h(), t())
      case _ => None
    }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = {
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
    }
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    zipWithAll(s2)((_, _))
  }

  def zipAll2[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    unfold((this, s2)) {
      case (s1, s22) => (s1, s22) match {
        case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
        case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (s1, t2()))
        case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), s22))
        case _ => None
      }
    }
  }

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C) = {
    unfold((this, s2)) {
      case (Empty, Cons(h2, t2)) => Some(f(None, Some(h2())) -> (empty[A], t2()))
      case (Cons(h1, t1), Empty) => Some(f(Some(h1()), None) -> (t1(), empty[B]))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1(), t2()))
      case _ => None
    }
  }

  //  exercise 5.13 page 76 end

  //  exercise 5.14 page 76 start
  //  wrong answer
  //  def startsWith[A](s: Stream[A]): Boolean = {
  //    (zipWith(s)((a, b) => (Some(a), Some(b))).foldRight(true) { (h, t) =>
  //
  //      if (h match {
  //        case (Some(h2), Some(t2)) => println(h2+","+t2) ;h2 == t2
  //        case _ => false
  //      }) t
  //      else
  //        false
  //    }
  //
  //      )
  //  }


  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty) forAll {
      case (h, h2) => h == h2
    }

  //  exercise 5.14 page 76 end

  //  exercise 5.15 page 76 start
  //  my answer
  def tails: Stream[Stream[A]] = {
    cons(this, unfold(this) {
      case Cons(h, t) => Some(t(), t())
      case _ => None
    })
  }

  //  除了 append Stream(empty) 寫法，其他很簡潔
  def tails2: Stream[Stream[A]] =
  unfold(this) {
    case Empty => None
    case s => Some((s, s drop 1))
  } append Stream(empty)

  //  exercise 5.15 page 76 end


  def hasSubsequence[A](s: Stream[A]) = {
    tails exists (_ startsWith s)
  }

  def hasSubsequence2[A](s: Stream[A]) = {
    tails2 exists (_ startsWith s)
  }

  //  exercise 5.16 page 77 start
  //  my wrong answer
  //  def scanRight[B](e: B)(f: (A, B) => B): Stream[B] = {
  //    foldRight(cons(e, empty))((a, b) => b match {
  //      case Cons(h, t) => cons(f(a, h()), t())
  //      case _ => empty
  //    })
  //  }

  /*
The function can't be implemented using `unfold`, since `unfold` generates elements of the `Stream` from left to right. It can be implemented using `foldRight` though.
The implementation is just a `foldRight` that keeps the accumulated value and the stream of intermediate results, which we `cons` onto during each iteration. When writing folds, it's common to have more state in the fold than is needed to compute the result. Here, we simply extract the accumulated list once finished.
*/
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
  foldRight((z, Stream(z)))((a, p0) => {
    // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
    lazy val p1 = p0
    val b2 = f(a, p1._1)
    (b2, cons(b2, p1._2))
  })._2


  //  exercise 5.16 page 77 end
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](h1: => A, t1: => Stream[A]): Stream[A] = {
    lazy val h = h1
    lazy val t = t1
    Cons(() => h, () => t)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty)
      Empty
    else {
      cons(as.head, apply(as.tail: _*))
    }
  }

  def headOption[A](s: Stream[A]) = s match {
    case Empty => None
    case Cons(h, t) => Some(h()) //Explicit forcing of then thunk using h()
  }

  //  exercise 5.8 page 74 start
  //  my answer
  def constant[A](a: A): Stream[A] = {
    cons(a, constant(a))
  }

  // This is more efficient than `cons(a, constant(a))` since it's just
  // one object referencing itself.
  def constantOneObject[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  //  exercise 5.8 page 74 end

  //  sharing recursive definition,
  lazy val ones: Stream[Int] = cons(1, ones)

  //  exercise 5.9 page 74 start
  def from(n: Int): Stream[Int] = {
    cons(n, from(n + 1))
  }

  //  exercise 5.9 page 74 end

  //  exercise 5.10 page 75 start
  def fibs: Stream[Int] = {
    def go(f0: Int, f1: Int): Stream[Int] = {
      cons(f0, go(f1, f0 + f1))
    }
    go(0, 1)
  }

  //  exercise 5.10 page 75 end

  //  exercise 5.11 page 75 start
  //  called corecursive
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty
    }
  }

  //  exercise 5.11 page 75 end

  //  exercise 5.12 page 75 start
  def fibsViaUnfold: Stream[Int] = {
    unfold((0, 1)) { case (f0, f1) => Some(f0, (f1, f0 + f1)) }
  }

  def fromViaUnfold(n: Int): Stream[Int] = {
    unfold(n)(s => Some(n, n + 1))
  }

  def constantViaUnfold[A](a: A): Stream[A] = {
    unfold(a)(_ => Some(a, a))
  }

  def oneViaUnfold: Stream[Int] = {
    //    constantViaUnfold(1)
    unfold(1)(_ => Some(1, 1))
  }

  //  exercise 5.12 page 75 end


}

