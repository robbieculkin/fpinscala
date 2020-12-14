package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc
import language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

//10.1
  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    val zero = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 * a2
    val zero = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 || a2
    val zero = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 && a2
    val zero = true
  }

//10.2
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(x: Option[A], y: Option[A]) = x orElse y
    val zero = None
  }
//10.3
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A=>A] {
    def op(x: A => A, y: A => A) = a => x(y(a))
    val zero = x => x
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }

//10.4
  import fpinscala.testing._
  import Prop._
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = 
    forAll(for {
      x <- gen
      y <- gen
      z <- gen
    } yield(x,y,z))(p =>
      // associative
      m.op(m.op(p._1, p._2), p._3) == m.op(p._1, m.op(p._2, p._3))
    ) && 
    forAll(gen)(a =>
      //identity
      (m.op(a, m.zero) == a)
      && (m.op(m.zero, a) == a)
    )


  def trimMonoid(s: String): Monoid[String] = ???

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  //10.5
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b,a)=> m.op(b, f(a)))

  //10.6
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(a => b => f(a,b))(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b,a))(z)

  //10.7
  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (as.length == 0) // base case empty
      m.zero
    else if (as.length == 1) // base case singleton
      f(as(0))
    else {
      val (x,y) = as.splitAt(as.length/2)
      m.op(foldMapV(x, m)(f),foldMapV(y,m)(f))
    }
  }

  //10.9
  val orderedMonoid = new Monoid[Option[(Int, Int, Boolean)]] {
    def op(x: Option[(Int, Int, Boolean)], y: Option[(Int, Int, Boolean)]) = (x,y) match {
      // xi tracks the min, yi tracks the max
      // p & q track whether elements seen so far are ordered
      case(Some((x1,y1,p)), Some((x2,y2,q))) =>
        Some(x1 min x2, y1 max y2, p && q && (y1 <= x2))
      case (x1, None) => x1
      case (None, x2) => x2
    }
    val zero = None
  }
  def ordered(ints: IndexedSeq[Int]): Boolean =
    foldMapV(ints, orderedMonoid)(
      i => Some((i,i,true))) // convert in to Option[(Int, Int, Boolean)]
    .map(_._3)  //grab resulting boolean from fold
    .getOrElse(true) // if empty then it's ordered by default, so true

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  //10.8
  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def zero = Par.unit(m.zero)
    def op(x: Par[A], y: Par[A]) = x.map2(y)(m.op)
  }

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = 
    Par.parMap(v)(f) flatMap { bs =>
      foldMapV(bs, par(m))(b => Par.lazyUnit(b))
    }

  // 10.10
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def zero = Stub("")
    def op(a: WC, b: WC) = (a, b) match {
      case (Part(al, ac, ar), Part(bl, bc, br)) => 
        Part(al, ac + bc + (if ((ar+bl).length() > 0) 1 else 0), br)
      case (Part(l, c, r), Stub(s)) => Part(l, c, r+s)
      case (Stub(s), Part(l, c, r)) => Part(s+l, c, r)
      case (Stub(s1),Stub(s2)) => Stub(s1+s2)
    }
  }
  //10.11
  def count(s: String): Int = {
    def unstub(a: String) = a.length min 1 // 0 if empty 1 otherwise
    foldMapV(s, wcMonoid)(c => 
      if (c.isWhitespace) Part("", 0, "") 
      else Stub(c.toString)
    ) match {
      case Stub(a) => unstub(a)
      case Part(l, w, r) => unstub(l) + w + unstub(r)
    }
  }
    
  //10.16
  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = 
    new Monoid[(A, B)] {
      val zero = (A.zero, B.zero)
      def op(l: (A,B), r: (A,B)) = (A.op(l._1, r._1), B.op(l._2, r._2))
    }
    
  //10.17
  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    def zero = (a:A) => B.zero
    def op(l: A=>B, r: A=>B) = (a:A) => B.op(l(a), r(a))
  }

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def zero = Map[K,V]()
      def op(a: Map[K, V], b: Map[K, V]) =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc,k) =>
          acc.updated(k, V.op(a.getOrElse(k, V.zero),
                      b.getOrElse(k, V.zero)))
        } 
    }

  //10.18
  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    val M: Monoid[Map[A, Int]] = mapMergeMonoid(intAddition)
    foldMapV(as, M)(a => Map(a -> 1))
  }   
}

//10.12
trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(a => (b:B) => f(a,b))(endoMonoid[B])(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b:B) => f(b,a))(dual(endoMonoid[B]))(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a,b) => mb.op(f(a),b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  //10.15
  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List[A]())((a:A, acc:List[A]) => a :: acc)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a,b) => mb.op( f(a), b ))
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  import Monoid._
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    foldMapV(as, mb)(f)
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  // no fold map V bc streams processed l -> r?
}
// 10.13
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Leaf(a) => f(a)
    case Branch(l,r) => mb.op(foldMap(l)(f)(mb),foldMap(r)(f)(mb))
  }
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) = as match {
    case Leaf(a) => f(z, a)
    case Branch(l,r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
  }
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) = as match {
    case Leaf(a) => f(a, z)
    case Branch(l,r) => foldRight(l)(foldRight(r)(z)(f))(f)
  }
}

//10.13
object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = 
    as map(f) getOrElse mb.zero
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) = as match {
    //possible to do with map2?
    case None => z
    case Some(a) => f(z,a)
  }
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) = as match {
    //possible to do with map2?
    case None => z
    case Some(a) => f(a,z)
  }
}

