package fpinscala
package monads

import parsing._
import testing._
import parallelism._
import state._
import parallelism.Par._
import language.higherKinds


trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]

  def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A,B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]
  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]

  def map[A,B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  //11.3
  def sequence[A](lma: List[M[A]]): M[List[A]] = 
    lma.foldRight(unit(List[A]()))((ma, mla) => map2(ma, mla)(_ :: _))

  def traverse[A,B](la: List[A])(f: A => M[B]): M[List[B]] = 
    la.foldRight(unit(List[B]()))((a, mla) => map2(f(a), mla)(_ :: _))

  //11.4
  def replicateM[A](n: Int, ma: M[A]): M[List[A]] = 
    sequence(List.fill(n)(ma)) // so clean!
  //11.5
  // For the List monad, this makes an n-long list of ma lists
  // For the Option monad, it unpacks the Option value n times and makes 
  // that whole thing an Option. If the value is None then the Option 
  // collapses to one None.
  // Generally, it unpacks whatever monad you give it, duplicates in a list,
  // then wraps the result in the monad

  def product[A,B](ma: M[A], mb: M[B]): M[(A, B)] = map2(ma, mb)((_, _))

  //11.6
  def filterM[A](as: List[A])(f: A => M[Boolean]): M[List[A]] = 
  as match { 
    case Nil => unit(Nil)
    case h :: t => flatMap(f(h))(bool => 
      if (!bool) filterM(t)(f)
      else map(filterM(t)(f))(m => h :: m)
    )
  }

  //11.7 correct but dumb implementaation
  def compose_dumb[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] =
    a => flatMap(flatMap(unit(a))(f))(g)
  //11.7 better implementation
  def compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] =
    a => flatMap(f(a))(g)
  // 11.8 Implement in terms of `compose`:
  def _flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = 
    compose((a:Unit) => ma, f)(()) // make first compose arg a constant function
  // 11.9 on paper
  // 11.10 on paper
  // 11.11 on paper
  // 11.12
  def join[A](mma: M[M[A]]): M[A] = 
    flatMap(mma)(ma => ma)

  // 11.13 Implement in terms of `join`:
  def __flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = 
    join(map(ma)(f))
}
//11.14 paper
//11.15 Par: inner nested threads need to finish before outers start
//11.16 Gen: Gen(f(x)) == f(Gen(x))
//  List: unit is a singleton list
case class Reader[R, A](run: R => A)

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  //11.1
  val parMonad: Monad[Par] = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.unit(a)
    def flatMap[A,B](ma: Par[A])(f: A => Par[B]): Par[B] =
      Par.flatMap(ma)(f)
  }

  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = ???

  val optionMonad: Monad[Option] = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)
    def flatMap[A,B](ma: Option[A])(f: A => Option[B]): Option[B] =
      ma flatMap f
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    def unit[A](a: => A): Stream[A] = Stream(a)
    def flatMap[A,B](ma: Stream[A])(f: A => Stream[B]): Stream[B] =
      ma flatMap f
  }

  val listMonad: Monad[List] = new Monad[List] {
    def unit[A](a: => A): List[A] = List(a)
    def flatMap[A,B](ma: List[A])(f: A => List[B]): List[B] =
      ma flatMap f
  }

  //11.2 uses tricky syntax: type members
  def stateMonad[S] = new Monad[({type f[x] = State[S,x]})#f] {
    def unit[A](a: => A): State[S,A] = State(s => (a, s))
    def flatMap[A,B](st: State[S,A])(f: A => State[S,B]): State[S,B] =
      st flatMap f 
  }
  //11.18 replicateM chains states together. 
  //  map2 composes states then passes them to f
  //  sequence chains states and puts them in a list

  val idMonad: Monad[Id] = new Monad[Id] {
    def unit[A](a: => A) = Id(a)
    def flatMap[A,B](ida: Id[A])(f: A => Id[B]): Id[B] = ida flatMap f
  }

  def readerMonad[R] = ???
}
//11.17
case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id((f(value)))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
    def unit[A](a: => A): Reader[R,A] = ???
    override def flatMap[A,B](st: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] = ???
  }
}

