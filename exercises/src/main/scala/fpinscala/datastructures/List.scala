package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  
  def str[A](l:List[A]): String = l match {
    case Nil => ""
    case Cons(h,Nil) => h.toString()
    case Cons(h,t) => h.toString()+ ","+str(t)
  }
  def printer[A](l:List[A]): Unit = println(str(l))
  
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail of empty list")
    case Cons(_,xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = Cons(h,tail(l))

  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else l match {
      case Cons(x,xs) => drop(tail(l), n-1)
      case Nil => Nil
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x,xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  def dropWhile2[A](l: List[A])(f:A=>Boolean): List[A] = l match {
    case Cons(x,xs) if f(x) => dropWhile2(xs)(f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x,Nil) => Nil
    case Cons(x,xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int = 
    foldRight(l, 0)((_,s) => 1+s)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x,xs) => foldLeft(xs,f(z,x))(f)
  }

  def sum3(l:List[Int]):Int = foldLeft(l,0)(_+_)
  def product3(l:List[Int]): Int = foldLeft(l,1)(_*_)
  def length2[A](l:List[A]): Int = foldLeft(l,0)((s,_)=>s+1)

  def reverse[A](l:List[A]): List[A] = foldLeft(l,Nil:List[A])((z,h)=>Cons(h,z))

  def foldLeftViaFoldRight[A,B](l:List[A],z: B)(f: (B, A) => B): B =
    foldRight(l, (b:B)=>b)((a,g)=>b=>g((f(b,a))))(z)

  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(reverse(l), z)((b,a) => f(a,b))
  
  def append2[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1,a2)(Cons(_,_))
  }

  def concat[A](ll:List[List[A]]): List[A] = foldRight(ll, Nil:List[A])(append)
  
  def add1(l:List[Int]):List[Int] = foldRight(l, Nil:List[Int])((h,t)=>Cons(1+h,t))
  
  def doubleString(l:List[Double]):List[String] = 
    foldRight(l, Nil:List[String])((h,t)=>Cons(h.toString,t))

  def map[A,B](l: List[A])(f: A => B): List[B] = 
    foldRightViaFoldLeft(l, Nil:List[B])((h,t)=>Cons(f(h),t))

  def filter[A](l:List[A])(f:A=>Boolean):List[A] =
    foldRightViaFoldLeft(l, Nil:List[A])((h,t)=>if (f(h)) Cons(h,t) else t)

  def flatMap[A,B](l:List[A])(f:A=>List[B]): List[B] = 
    foldRightViaFoldLeft(l, Nil:List[B])((h,t)=>append(f(h),t))

  def flatMap_1[A,B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  def filterViaFlatMap[A](l:List[A])(f:A=>Boolean):List[A] = 
    flatMap(l)((h:A) => if (f(h)) List(h) else List[A]())

  def addElements(l1:List[Int], l2:List[Int]):List[Int] = (l1,l2) match {
    case (Nil,_) => Nil
    case (_,Nil) => Nil
    case (Cons(a,t1), Cons(b,t2)) => Cons(a+b, addElements(t1,t2))
  }

  def zipWith[A,B,C](a:List[A], b:List[B])(f:(A,B)=>C):List[C] = (a,b) match {
    case (Nil,_) => Nil
    case (_,Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
  }
}

object ListDS {
  import List._

  def main(args: Array[String]): Unit = {
    val l = List(1,2,3,4)
    printer(init(l))
    //must specify type of x for anon function
    printer(dropWhile(l, (x:Int) => x < 3))
    //can now leave types unspecified thanks to inference
    printer(dropWhile2(l)(_ < 3))
    val r = List("rob", "bob")
    printer(r)
    //does nothing
    printer(foldRight(l,Nil:List[Int])(Cons(_,_)))
    println(length(l))
    println(sum3(l))
    printer(reverse(l))
    printer(append2(l,Cons(0,l)))
    // map is useful
    printer(add1(l))
    printer(map(l)(_+1))
    //filters out odds
    printer(filter(l)(_%2==0))
    printer(filterViaFlatMap(l)(_%2==0))
    //makes 2 of every element
    printer(flatMap(l)(i=>List(i,i)))
    //add pairwise
    printer(addElements(l,l))
    printer(zipWith(l,l)(_+_))
  }
}