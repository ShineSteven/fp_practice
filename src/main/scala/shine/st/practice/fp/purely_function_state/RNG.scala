package shine.st.practice.fp.purely_function_state

import scala.annotation.tailrec

/**
  * Created by shinest on 2016/9/30.
  */
trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  //  exercise 6.1 page 82 start
  //my answer wrong, Since `Int.Minvalue` is 1 smaller than `-(Int.MaxValue)`

  //  def nonNegativeInt(rng: RNG): (Int, RNG) = {
  //    val v = rng.nextInt
  //    (Math.abs(v._1), v._2)
  //  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (v, r) = rng.nextInt
    (if (v < 0) -(v + 1) else v, r)
  }

  //  exercise 6.1 page 82 end

  //  exercise 6.2 page 83 start
  //  because not include 1, int.maxvalue need plus 1
  def double(rng: RNG): (Double, RNG) = {
    val (v, r) = nonNegativeInt(rng)
    (v / Int.MaxValue.toDouble + 1, r)
  }

  //  exercise 6.2 page 83 end

  //  exercise 6.3 page 82 start
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = nonNegativeInt(rng)
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  //  exercise 6.3 page 83 end

  //  exercise 6.4 page 83 start
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {

    if (count <= 0)
      (Nil, rng)
    else {
      val (x1, r1) = rng.nextInt
      val (x2, r2) = ints(count - 1)(r1)
      (x1 :: x2, r2)
    }
  }

  //  tail recursion
  def intsViaTail(count: Int)(rng: RNG): (List[Int], RNG) = {

    @annotation.tailrec
    def go(n: Int, xs: List[Int], r: RNG): (List[Int], RNG) = {
      if (n < count) {
        val (x, sr) = r.nextInt
        go(n + 1, x :: xs, sr)
      } else
        (xs, r)
    }

    go(0, Nil, rng)

  }

  //  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
  //    def go(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) =
  //      if (count <= 0)
  //        (xs, r)
  //      else {
  //        val (x, r2) = r.nextInt
  //        go(count - 1, r2, x :: xs)
  //      }
  //    go(count, rng, List())
  //  }

  //  exercise 6.4 page 83 end

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng =>
      val (a, r) = s(rng)
      (f(a), r)
  }

  def nonNegativeEven: Rand[Int] = {
    map(nonNegativeInt)(i => i - i % 2)
  }

  //  exercise 6.5 page 85 start
  val doubleViaMap: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))


  //  exercise 6.5 page 85 end


  //  exercise 6.6 page 85 start
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng =>
      val (a, r2) = ra(rng)
      val (b, r3) = rb(r2)
      (f(a, b), r3)
  }

  //  exercise 6.6 page 85 end

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = {
    map2(ra, rb)((_, _))
  }

  val randIntDouble: Rand[(Int, Double)] = both(nonNegativeInt, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, nonNegativeInt)

  //  exercise 6.7 page 85 start
  //  my answer, 不夠簡潔
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng =>
      val result = fs.foldRight((List.empty[A], rng)) { (r, a) =>
        val (v, r1) = r(a._2)
        (v :: a._1, r1)
      }
      (result._1, result._2)
  }


  // graceful answer
  def sequence_1[A](fs: List[Rand[A]]): Rand[List[A]] =
  fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  //  exercise 6.7 page 85 end

  //  exercise 6.8 page 87 start
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    val (a, r1) = f(rng)
    g(a)(r1)
  }

  //  my answer
  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { a => rng =>
      val mod = a % n
      if (a + (n - 1) - mod >= 0)
        (mod, rng)
      else
        nonNegativeLessThan(n)(rng)
    }
  }

  // graceful answer
  def nonNegativeLessThan2(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { a =>
      val mod = a % n
      if (a + (n - 1) - mod >= 0)
        unit(mod)
      else
        nonNegativeLessThan2(n)
    }
  }

  //  exercise 6.8 page 87 end


  //  exercise 6.9 page 87 start
  def _map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(a => unit(f(a)))
  }

  //  my answer
  //  def _map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
  //    flatMap(ra) { rav => r1 =>
  //      val (rbv, r2) = rb(r1)
  //      (f(rav, rbv), r2)
  //    }
  //  }

  // graceful
  def _map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => map(rb)(b => f(a, b)))
  }

  //  exercise 6.9 page 87 end


  def rollDie = map(nonNegativeLessThan(6))(_+1)
  import scala.util.Random
  Random.nextInt()
}

