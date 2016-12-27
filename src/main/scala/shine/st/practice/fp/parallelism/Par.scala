package shine.st.practice.fp.Parsallelism

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}


/**
  * Created by shinest on 14/12/2016.
  */


object Par {
  type Par[A] = ExecutorService => Future[A]


  private case class UnitFuture[A](get: A) extends Future[A] {
    override def isCancelled: Boolean = false

    override def get(timeout: Long, unit: TimeUnit): A = get

    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    override def isDone: Boolean = true
  }


  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def fork[A](a: => Par[A]): Par[A] = (es: ExecutorService) => {
    es.submit(new Callable[A] {
      override def call = a(es).get
    })
  }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = {
    map2(pa, unit(()))((a, _) => f(a))
    //    (es:ExecutorService) => { UnitFuture(f(p1(es).get)) }
  }


  def map2[A, B, C](p1: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
    UnitFuture(f(p1(es).get, p2(es).get))
  }


  def id[A](a: A): A = a

  def test() = {
    //    val a = unit(1)
    val b = map(unit(1))(id)
    //    b
  }


  //  def sum(ints: IndexedSeq[Int]): Par[Int] = {
  //    if (ints.size <= 1)
  //      Par.unit(ints.headOption getOrElse 0)
  //    else {
  //      val (l, r) = ints.splitAt(ints.length / 2)
  //  sum(l) sum(r) 分開為了能做 parallelism
  //      Par.map2(sum(l), sum(r))(_ + _)
  //    }
  //  }

  //  Exercise 7.1 on page 227
  /* def map2[A, B, C](p1: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] */

  //  Exercise 7.3 on page 241
  /* hint
    In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating one future,
     then subtracts that time from the available time allocated for evaluating the other future.
   */
  //  my answer
  private case class Map2FutureMe[A, B, C](f1: Future[A], f2: Future[B], f: (A, B) => C) extends Future[C] {
    override def isCancelled: Boolean = f1.isCancelled || f2.isCancelled

    override def get(): C = get(Long.MaxValue, TimeUnit.MILLISECONDS)

    override def get(timeout: Long, un: TimeUnit): C = {
      val unit = TimeUnit.MILLISECONDS.convert(timeout, un)

      val start = System.currentTimeMillis()
      val r1 = f1.get
      val r1End = System.currentTimeMillis() - start
      if (unit - r1End < 0)
        throw new Exception("over all")

      val r2 = f2.get
      val r2End = System.currentTimeMillis() - r1End
      if ((unit - r1End) - r2End < 0)
        throw new Exception("over all")

      f(r1, r2)
    }

    override def cancel(mayInterruptIfRunning: Boolean): Boolean = f1.cancel(mayInterruptIfRunning) || f2.cancel(mayInterruptIfRunning)

    override def isDone: Boolean = f1.isDone && f2.isDone
  }

  //  correct answer
  /*
  Note: this implementation will not prevent repeated evaluation if multiple threads call `get` in parallel. We could prevent this using synchronization, but it isn't needed for our purposes here (also, repeated evaluation of pure values won't affect results).
  */
  case class Map2Future[A, B, C](a: Future[A], b: Future[B],
                                 f: (A, B) => C) extends Future[C] {
    @volatile var cache: Option[C] = None

    def isDone = cache.isDefined

    def isCancelled = a.isCancelled || b.isCancelled

    def cancel(evenIfRunning: Boolean) =
      a.cancel(evenIfRunning) || b.cancel(evenIfRunning)

    def get = compute(Long.MaxValue)

    def get(timeout: Long, units: TimeUnit): C =
      compute(TimeUnit.NANOSECONDS.convert(timeout, units))

    private def compute(timeoutInNanos: Long): C = cache match {
      case Some(c) => c
      case None =>
        val start = System.nanoTime
        val ar = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
        val stop = System.nanoTime;
        val aTime = stop - start
        val br = b.get(timeoutInNanos - aTime, TimeUnit.NANOSECONDS)
        val ret = f(ar, br)
        cache = Some(ret)
        ret
    }
  }

  def map2Future[A, B, C](p1: Par[A], p2: Par[B])(timeout: Long, unit: TimeUnit)(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
    Map2Future(p1(es), p2(es), f)
  }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }


  //  Exercise 7.4 on page 241
  def asyncF[A, B](f: A => B): A => Par[B] = a => {
    lazyUnit(f(a))
  }

  //  Exercise 7.5 on page 246
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldLeft(Par.unit(List.empty[A]))((r, p) => map2(p, r)(_ :: _))
  }

  //  another answer
  def sequenceRight[A](ps: List[Par[A]]): Par[List[A]] = {
    ps match {
      case Nil => unit(List.empty)
      case h :: t => map2(h, sequenceRight(t))(_ :: _)
    }
  }

  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) unit(Vector())
    else if (as.length == 1) map(as.head)(a => Vector(a))
    else {
      val (l, r) = as.splitAt(as.length / 2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def sequence_B[A](as: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(as.toIndexedSeq))(_.toList)

  //  Exercise 7.6 on page 246
  //  wrong
  //  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
  //    lazyUnit(as.filter(f))
  //  }

  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] =
      l map (asyncF((a: A) => if (f(a)) List(a) else List()))
    map(sequence(pars))(_.flatten) // convenience method on `List` for concatenating a list of lists
  }


  //  Exercise 7.7 on page 246
  //  prove it
  //  Given map(y)(id) == y, it’s a free theorem that map(map(y)(g))(f) == map(y)(f compose g)

  //  map(y)(f) == y
  //  map(unit(z))(f) == unit(z)
  //  map(map(unit(z))(g))(f) == unit(g(z))
  //  map(unit(g(z)))(f) == unit(f(g(z)))
  //  map(unit(g(z)))(f) == unit(f compose g (z))
  //  map(unit(g(z)))(f) == map(unit(z))(f compose g)
  //  map(map(unit(z))(g))(f) == map(unit(z))(f compose g)
  //  map(map(y)(g))(f) == map(y)(f compose g)


}