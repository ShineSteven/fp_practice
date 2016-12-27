package shine.st.practice.fp.data_structures

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty)
      Nil
    else
      Cons(as.head, apply(as.tail: _*))

  //exercise 3.1 page 34
  //  answer: 3

  //exercise 3.2 page 35
  def tail[A](list: List[A]): List[A] = list match {
    // wrong
    // case Nil => Nil
    case Nil => sys.error("tail of empty list")
    case Cons(_, xs) => xs
  }

  // exercise  3.3 page 36
  def setHead[A](list: List[A], head: A): List[A] = list match {
    //wrong
    //case Nil => Nil
    case Nil => sys.error("setHead on empty list")
    case Cons(_, xs) => Cons(head, xs)
  }

  //exercise 3.4 page 36
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) if n > 0 => drop(xs, n - 1)
    case _ => l
  }

  //another answer
  //  def drop[A](l: List[A], n: Int): List[A] = {
  //    if (n <= 0) l
  //    else l match {
  //      case Nil => Nil
  //      case Cons(_,t) => drop(t, n-1)
  //    }
  //  }

  //exercise 3.5 page 36
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(head, tail) if (f(head)) => dropWhile(tail, f)
      case _ => l
    }
  }

  //exercise 3.6 page 37
  //wrong all
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }


  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  //exercise 3.7.1 page 40
  //wrong
  //answer: yes
  def product2(ns: List[Double]) =
    foldRight(ns, 1.0) { (x, y) => if (x == 0.0 || y == 0.0)
      throw new Exception("number is 0.0")
    else
      x * y
    }

  //exercise 3.7.2 page 40
  //answer: no tail-recursive so may short-circuiting

  //correct answer
  //  No, this is not possible! The reason is because _before_ we ever call our function, `f`, we evaluate its argument, which in the case of `foldRight` means traversing the list all the way to the end. We need _non-strict_ evaluation to support early termination---we discuss this in chapter 5.


  //exercise 3.8 page 40
  //answer: same thing

  //exercise 3.9 page 40
  def length[A](as: List[A]): Int = {
    foldRight(as, 0) { (_, y) => 1 + y }
  }

  //exercise 3.10 page 40
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  //exercise 3.11 page 41
  def sum2(as: List[Int]): Int = {
    foldLeft(as, 0)(_ + _)
  }

  def product3(as: List[Double]): Double = {
    foldLeft(as, 1.0)(_ * _)
  }

  def length2[A](l: List[A]): Int = foldLeft(l, 0) { (a, _) => a + 1 }

  //exercise 3.12 page 41
  def reverse[A](as: List[A]): List[A] = {
    //wrong
    //    as match {
    //      case Nil => Nil
    //      case _ => foldRight[A, List](as, Nil) { (a, b) => Cons(a, b) }
    //    }

    //correct answer
    foldLeft(as, List[A]())((acc, h) => Cons(h, acc))

    //another answer
    foldLeft(as, Nil: List[A])((acc, h) => Cons(h, acc))
  }

  //exercise 3.13 page 41
  //correct answer
  def foldRightViaFoldLeft_1[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(l, (b: B) => b)((g, a) => (b => g(f(a, b))))(z)
  }

  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(l, (b: B) => b)((a, g) => (b => g(f(b, a))))(z)
  }

  //exercise 3.14 page 41
  def append[A](l: List[A], itemList: List[A]) = {
    foldRight(l, itemList) { (a, b) => Cons(a, b) }

    //    book answer
    //    foldRight(l, r)(Cons(_,_))
  }

  //exercise 3.15 page 41
  // wrong answer
  //  def concatenates[A](l: List[A], t: List[A], f: A => Boolean): List[A] = {
  //    l match {
  //      case Nil => Nil
  //      case Cons(x, xs) if f(x) => Cons(x, concatenates(xs, t, f))
  //      case _ => t
  //    }
  //  }

  //correct answer
  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])(append)


  //  exercise 3.16 page 42
  def transform(as: List[Int]): List[Int] = {
    foldRight(as, Nil: List[Int]) { (a, b) => Cons(a + 1, b) }
  }

  //  exercise 3.17 page 42
  def transform2(as: List[Double]): List[String] = {
    foldRight(as, Nil: List[String]) { (a, b) => Cons(a.toString, b) }
  }

  //  exercise 3.18 page 42
  def map[A, B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, Nil: List[B]) { (a, b) => Cons(f(a), b) }
  }

  //  another answer
  def map_1[A, B](l: List[A])(f: A => B): List[B] =
    foldRightViaFoldLeft_1(l, Nil: List[B])((h, t) => Cons(f(h), t))

  def map_2[A, B](l: List[A])(f: A => B): List[B] = {
    val buf = new collection.mutable.ListBuffer[B]
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h, t) => buf += f(h); go(t)
    }
    go(l)
    List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
  }

  //  exercise 3.19 page 42
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, Nil: List[A]) { (a, b) =>
      if (f(a))
        Cons(a, b)
      else
        b
    }
  }

  //    another answer
  def filter_1[A](l: List[A])(f: A => Boolean): List[A] =
    foldRightViaFoldLeft_1(l, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  def filter_2[A](l: List[A])(f: A => Boolean): List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h, t) => if (f(h)) buf += h; go(t)
    }
    go(l)
    List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
  }


  //  exercise 3.20 page 42
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight(as, Nil: List[B]) { (a, b) => append(f(a), b) }
  }

  //  git simple answer
  def flatMap1[A, B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  //  For instance, flatMap(List(1,2,3))(i => List(i,i)) should result in
  //    List(1,1,2,2,3,3).

  //  exercise 3.21 page 43
  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as) { a => if (f(a))
      List(a)
    //      Cons(a,Nil)
    else
      Nil
    }
  }


  //exercise 3.22 page 43
  //correct answer
  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
  }

  //exercise 3.23 page 43
  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = {
    (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }
  }

  //  exercise 3.24 page
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def startHas[A](sup: List[A], sub: List[A]): Boolean = {
      (sup, sub) match {
        case (_, Nil) => true
        case (Cons(h1, t1), Cons(h2, t2))
          if (h1 == h2) => startHas(t1, t2)
        case _ => false
      }
    }

    sup match {
      case Nil => sub == Nil
      case _ if startHas(sup, sub) => true
      case Cons(_, t) => hasSubsequence(t, sub)
    }
  }
}

