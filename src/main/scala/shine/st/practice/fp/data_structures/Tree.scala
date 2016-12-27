package shine.st.practice.fp.data_structures

/**
  * Created by stevenfanchiang on 2016/4/8.
  */
sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeExercise {
  //exercise 3.25 page 46
  def size[A](tree: Tree[A]): Int = tree match {
    case Branch(left, right) => 1 + size(left) + size(right)
    case Leaf(_) => 1
  }

  //exercise 3.26 page 46
  def maximum(tree: Tree[Int]): Int = tree match {
    case Branch(left, right) => maximum(left) max maximum(right)
    case Leaf(v) => v
  }

  //exercise 3.27 page 46
  //  def depth[A](tree: Tree[A): Int = tree match {
  //    case Branch(left, right) => (1 + size(left)) max (1 + size(right))
  //    case Leaf(_) => 1
  //  }

  //correct answer
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  //exercise 3.28 page 46
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    case Leaf(v) => Leaf(f(v))
  }

  //exercise 3.29 page 47
  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
    case Leaf(v) => f(v)
  }

  def sizeViaFold[A](tree: Tree[A]): Int = {
    fold(tree)(v => 1)(1 + _ + _)
  }

  def maximumViaFold(tree: Tree[Int]): Int = {
    fold(tree)(a => a)(_ max _)
  }

  def depthViaFold[A](tree: Tree[A]): Int = {
    fold(tree)(v => 0)(1 + _ max _)
  }

  def mapViaFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    fold(tree)(v => Leaf(f(v)): Tree[B])(Branch(_, _))
  }

}
