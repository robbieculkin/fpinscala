//  7.1
//  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B)=>C): Par[C]

//  7.2
//  something in java.util.concurrent

package fpinscala.parallelism

import java.util.concurrent._
import language.implicitConversions

object Par {
  type Par[A] = ExecutorService => Future[A]
  
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) 
// `unit` is represented as a function that returns a `UnitFuture`, 
// which is a simple implementation of `Future` that just wraps a constant value. 
// It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. 
// Its `get` method simply returns the value that we gave it.
  
  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true 
    def get(timeout: Long, units: TimeUnit) = get 
    def isCancelled = false 
    def cancel(evenIfRunning: Boolean): Boolean = false 
  }
  
  // `map2` doesn't evaluate the call to `f` in a separate logical thread, 
  // in accord with our design choice of having `fork` be the sole function 
  // in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))`
  //  if we want the evaluation of `f` to occur in a separate thread.
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = 
    (es: ExecutorService) => {
      val af = a(es) 
      val bf = b(es)
      UnitFuture(f(af.get, bf.get)) 
    }
  // This implementation of `map2` does _not_ respect timeouts, and eagerly waits 
  // for the returned futures. This means that even if you have passed in "forked" 
  // arguments, using this map2 on them will make them wait. It simply passes the 
  // `ExecutorService` on to both `Par` values, waits for the results of the Futures 
  // `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order 
  // to respect timeouts, we'd need a new `Future` implementation that records the 
  // amount of time spent evaluating `af`, then subtracts that time from the available 
  // time allocated for evaluating `bf`.

  // 7.3 - needs a Map2Future implementation that requires knowledge of the java concurrency utils
  // def map2_2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = 
  //   (es: ExecutorService) => {
  //     val af = a(es) 
  //     val bf = b(es)
  //     Map2Future(f(af.get, bf.get)) 
  //   }

  // This is the simplest and most natural implementation of `fork`, but there are some 
  // problems with it--for one, the outer `Callable` will block waiting for the "inner" 
  // task to complete. Since this blocking occupies a thread in our thread pool, or 
  // whatever resource backs the `ExecutorService`, this implies that we're losing out on 
  // some potential parallelism. Essentially, we're using two threads when one should suffice. 
  // This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
  def fork[A](a: => Par[A]): Par[A] = 
    es => es.submit(new Callable[A] { 
      def call = a(es).get
    })

  def map[A,B](pa: Par[A])(f: A => B): Par[B] = 
    map2(pa, unit(()))((a,_) => f(a))

  // needed for 7.4; impl wasn't included
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  // 7.4
  def asyncF[A,B](f: A => B): A => Par[B] = 
    a => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  // 7.5
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps match {
    case Nil => unit(Nil)
    case h :: t => map2(h, fork(sequence(t)))(_ :: _)
  }

  // needed for 7.6
  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork { 
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  // 7.6
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] = 
      as map (asyncF(( a:A ) => if (f(a)) List(a) else List()))
    map(sequence(pars))(_.flatten)
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = 
    p(e).get == p2(e).get

  // 7.7
  // on paper

  // 7.8
  // if the threadpool is small deadlocks occur

  // 7.9 
  // if you nest fork calls eg. fork(fork(fork...)) it will run out any threadpool eventually

  def delay[A](fa: => Par[A]): Par[A] = 
    es => fa(es)

  // 7.10 skipped

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es => 
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  // 7.11
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = 
    es => choices(run(es)(n).get)(es)

  def choiceViaChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(x => if(x) 1 else 0))(List(f,t))
    
  // 7.12
  def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] =
    es => run(es)(choices(run(es)(key).get))

  // 7.13
  def chooser[A, B](f: Par[A])(choices: A => Par[B]): Par[B] =
    es => run(es)(choices(run(es)(f).get))

  // weird way to do it but wanted to try out Map
  def choiceViaChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser(cond)(Map[Boolean, Par[A]](true->t, false->f)) 

  def choiceNViaChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = 
    chooser(n)(i => choices(i))

  def flatMap[A,B](p: Par[A])(choices: A => Par[B]): Par[B] =
    es => {
      val k = run(es)(p).get
      run(es)(choices(k))
    }

  // 7.14
  def join[A](a: Par[Par[A]]): Par[A] = 
    es => run(es)(run(es)(a).get())

  def flatMapViaJoin[A, B](f: Par[A])(choices: A => Par[B]): Par[B] =
    join(map(f)(choices))

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {


  }
}

object Examples {
  import Par._
  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else { 
      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

}
