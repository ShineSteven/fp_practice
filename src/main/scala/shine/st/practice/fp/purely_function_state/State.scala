package shine.st.practice.fp.purely_function_state

/**
  * Created by shinest on 2016/10/3.
  */

case class State[S, +A](run: S => (A, S)) {
  //  exercise 6.10 page 88 start
  def map[B](f: A => B): State[S, B] = {
    flatMap(a => State.unit(f(a)))
  }

  def map2[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] = {
    flatMap(a => rb.map(b => f(a, b)))
  }

  def flatMap[B](g: A => State[S, B]): State[S, B] = State { s =>
    val (a, s1) = run(s)
    g(a).run(s1)
  }

  //  exercise 6.10 page 88 end
}

object State {
  //  exercise 6.10 page 88 start
  def unit[S, A](a: A): State[S, A] = State { s => (a, s) }

  //  def map[S, A, B](state: State[S, A])(f: A => B): State[S, B] = {
  //    flatMap(state)(a => unit(f(a)))
  //  }
  //
  //  def map2[S, A, B, C](ra: State[S, A], rb: State[S, B])(f: (A, B) => C): State[S, C] = {
  //    flatMap(ra)(a => map(rb)(b => f(a, b)))
  //  }
  //
  //  def flatMap[S, A, B](state: State[S, A])(g: A => State[S, B]): State[S, B] = State { s =>
  //    val (a, r1) = state.run(s)
  //    g(a).run(r1)
  //  }

  //  my answer
  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
    fs.foldRight(unit[S, List[A]](List())) { (a, acc) => a.map2(acc)(_ :: _) }
  }

  //  another answer
  def sequenceViaFoldLeft[S, A](l: List[State[S, A]]): State[S, List[A]] =
  l.reverse.foldLeft(unit[S, List[A]](List()))((acc, f) => f.map2(acc)(_ :: _))


  def sequenceViaTailRecursive[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    def go(s: S, actions: List[State[S, A]], acc: List[A]): (List[A], S) =
      actions match {
        case Nil => (acc.reverse, s)
        case h :: t => h.run(s) match {
          case (a, s2) => go(s2, t, a :: acc)
        }
      }
    State((s: S) => go(s, sas, List()))
  }

  //  exercise 6.10 page 88 end

  type Rand[+A] = State[RNG, A]

  //可視為只定義型態
  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] =
    get.flatMap(s => set(f(s)))

  //  下面是用for的寫法
  //    for {
  //      s <- get
  //      _ <- set(f(s))
  //    } yield ()
}
