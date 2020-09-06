package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h,t) => h() :: t().toList
  }

  def toListTailRec: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h,t) => go(t(), h() :: acc)
      case _ => acc
    }
    go(this, List()).reverse
  }

  def take(n: Int): Stream[A] = this match {
      case Cons(h,t) if (n>0) => cons(h(), t().take(n-1))
      case Cons(h,t) if (n==0)=> cons(h(), empty)
      case _ => empty
    }

  def drop(n: Int): Stream[A] = this match {
      case Cons(_,t) if (n>0) => t().drop(n-1)
      case _ => this
    }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(h,t) => {
        lazy val head = h() //save double eval of h
        if (p(head))
          cons(head,t().takeWhile(p))
        else empty
        }
      case _ => empty
  }

  // checks that all elements in the Stream match a given predicate
  // terminate the traversal as soon as it encounters a nonmatching value
  def forAll(p: A => Boolean): Boolean = 
    foldRight(true)((a,b)=> p(a) && b)

  def takeWhileViaFoldRight(p: A=>Boolean): Stream[A] = 
    foldRight(empty[A])((a,b) => if (p(a)) cons(a,b) else empty)

  //hint: Let `None: Option[A]` be the first argument to `foldRight`. 
  def headOption: Option[A] = 
    foldRight(None: Option[A])((a,b) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f:A => B): Stream[B] = 
    foldRight(empty[B])((a,b)=> cons(f(a), b))

  def filter(p: A=>Boolean): Stream[A] = 
    foldRight(empty[A])((a,b)=> if(p(a)) cons(a,b) else b)

  def append[B>:A](other: => Stream[B]): Stream[B] = 
    foldRight(other)((a,b)=> cons(a,b))

  def flatmap[B](f:A => Stream[B]): Stream[B] = 
    foldRight(empty[B])((a,b)=> f(a) append b)

    //5.13
  def mapViaUnfold[B](f:A => B): Stream[B] = 
    unfold(this){
      case Cons(a,b) => Some((f(a()), b()))
      case _ => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((n,this)){
      case (n, Cons(a,b)) if n>=0 => Some((a(),(n-1,b())))
      case _ => None
    }

  def takeWhileViaUnfold(p: A=>Boolean): Stream[A] = 
    unfold(this){
      case Cons(a,b) if p(a()) => Some((a(),b()))
      case _ => None
    }

  // def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = 
  //   unfold((this,s2)){
  //     case (Empty, Empty) => None
  //     case (Cons(h, t), Empty) => Some((Some(h()), Option.empty[B]) -> (t(), empty[B]))
  //     case (Empty, Cons(h, t)) => Some((Option.empty[A], Some(h())) -> (empty[A] -> t()))
  //     case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())) -> (t1() -> t2()))
  //   }

  //5.14
  def startsWith[B](s: Stream[B]): Boolean = 
    zipWith(this, s)(_==_) match {
      case z if z.toList.length == s.toList.length => z.forAll(a=>a)
      case _ => false
    }

  //5.15
  def tails: Stream[Stream[A]] = 
    unfold(this){
      case Cons(h,t) => Some((t(), cons(h(),t())))
      case _ => None
    }

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  //5.16
  // def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
  //   tails map foldRight(z)(f)
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def constant[A](a:A): Stream[A] = 
    Stream.cons(a, constant(a))

  // This is more efficient than `cons(a, constant(a))` since it's just
  // one object referencing itself.
  def constant_2[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail) 
    tail
  }

  def from(n: Int): Stream[Int] = 
    cons(n, from(n+1))

  def fibs: Stream[Int] = {
    def loop(a:Int, b:Int): Stream[Int] = 
      cons(a+b, loop(b, a+b))
    loop(0,1)
  }

  //5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a,b)) => cons(a, unfold(b)(f))
    case None => empty[A]
  }
  
  //5.12
  def fibsViaUnfold: Stream[Int] =
    unfold((0,1))({
      case (a,b) => Some((a,(b, a+b)))
    })

  def fromViaUnfold(n:Int): Stream[Int] = 
    unfold(n)(a=>Some((a, a+1)))

  def constantViaUnfold[A](a:A): Stream[A] = 
    unfold(a)(a=>Some((a,a)))

  val onesViaUnfold = constantViaUnfold(1)

  //5.13
  def zipWith[A,B,C](a: Stream[A], b: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((a,b)){
      case (Cons(a0,a1), Cons(b0,b1)) => Some((f(a0(),b0()), (a1(),b1())))
      case _ => None
    }

}

object StreamTest {
  def main(args: Array[String]): Unit = {
    println(constant(5).take(5).toList)
    println(constantViaUnfold(5).take(5).toList)
    println(ones.take(5).toList)
    println(onesViaUnfold.take(5).toList) 
    println(onesViaUnfold.takeViaUnfold(5).mapViaUnfold(_+1).toList) 
    println(from(2).takeWhileViaUnfold(_<=4).toList)
    println(Stream(1,2,3).startsWith(Stream(1,2)))
    println(Stream(2,3).startsWith(Stream(1,2)))
    println(Stream(1,2).startsWith(Stream(1,2,3)))
    println(Stream(1,2,3,4,5).hasSubsequence(Stream(2,3)))
    println(from(1).hasSubsequence(Stream(2,3)))
    //stack overflows. no positive result to shortcircuit on
    //println(from(4).hasSubsequence(Stream(2,3))) 
  }
}