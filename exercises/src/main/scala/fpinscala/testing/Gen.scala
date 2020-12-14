package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/


// 8.1
// l => sum(l) == sum(l.reverse)
// sameVal => sum(sameVal)/sameVal.len == sameVal.headOption
//  or: sum(List.fill(n)(x)) == n*x
// emptyL => if (emptyL.len == 0) sum(emptyL) == 0 else true

// 8.2
// max(emptyL) == Null?
// max(singleItem) == singleItem.headOption
// l.map(x => max(l) >= x)
// max(l) is in l

// 8.18
// s.takeWhile(f).forall(f) == true
// List().takeWhile(x => false) == List()
// s.takeWhile(f) + s.dropWhile(f) == s

// 8.19
// hash function. take bit representation of arguments, caste as int.
// Do some mod function on this int to make an int of fixed size
// pass the int to an RNG

sealed trait Result {
  def isFalsified: Boolean
}
case object Passed extends Result {
  def isFalsified = false
}
case class Falsified(failure: FailedCase,
                     successes: SuccessCount) extends Result {
  def isFalsified = true
}
case object Proved extends Result {
  def isFalsified = false
}

case class Prop(run: (MaxSize,TestCases,RNG) => Result) {
  // 8.9; modified later
  def &&(p: Prop): Prop = Prop {
    (m, n, rng) => run(m, n, rng) match {
      case Passed => p.run(m, n, rng)
      case Proved => p.run(m, n, rng)
      case f => f //return falsified object
    }
  }
  def ||(p: Prop): Prop = Prop {
    (m, n, rng) => run(m, n, rng) match {
      case Falsified(f, s) => p.run(m, n, rng)
      case s => s
    }
  }
}

object Prop {
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int
  type FailedCase = String
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = 
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e:Exception): String = 
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}"+
    s"stack trace:\n${e.getStackTrace.mkString("\n")}"

  // old version with no max
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = Prop {
    (_,n,rng) => randomStream(gen)(rng).zip(Stream.from(0)).take(n).map {
      case (a,i) =>
        try {
          if (f(a)) Passed
          else Falsified(a.toString, i)
        }
        catch {
          case e: Exception => Falsified(buildMsg(a,e), i)
        }
    }.find(_.isFalsified).getOrElse(Passed)
  }
  // new version with max
  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, n, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max,n,rng)
  }
  // SGen version
  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def check(p: => Boolean): Prop = Prop { (_, _, _) => 
    if (p) Passed else Falsified("()", 0)
  }

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }

  val S = weighted(
    choose(1,4).map(Executors.newFixedThreadPool) -> .75,
    unit(Executors.newCachedThreadPool) -> .25)

  // def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
  //   forAll(S.map2(g)((_,_))) { case (s,a) => f(a)(s).get }

}

case class Gen[+A](sample: State[RNG,A]) {
  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))
  // 8.6
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(n: Int): Gen[List[A]] = 
    Gen.listOfN(n, this)
  
  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap (n => this.listOfN(n))

  // 8.10
  def unsized: SGen[A] = SGen(_ => this) //convert Gen to SGen

}


object Gen {
  // 8.4
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(i => i%(stopExclusive - start) + start))
  // 8.5
  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))
  
  def boolean: Gen[Boolean] = 
    Gen(State(RNG.boolean))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

    // 8.7
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = 
    boolean.flatMap(b => if(b) g1 else g2)

  // 8.8
  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = 
    Gen(State(RNG.double).flatMap(d => 
      if (d < (g1._2.abs / (g1._2.abs + g2._2.abs))) g1._1.sample
      else g2._1.sample
    ))

  // 8.12
  def listOf[A](g: Gen[A]): SGen[List[A]] = 
    SGen(listOfN(_,g))

  // 8.13
  def listOf1[A](g: Gen[A]): SGen[List[A]] = 
    SGen(n => listOfN(n.max(1),g)) //choose 1 or n, whichever is bigger

  val smallInt = Gen.choose(-10,10)
  val maxProp = forAll(listOf1(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }

  // 8.14
  val sortedProp = forAll(listOf(smallInt)) { l =>
    val s = l.sorted
    (
      (s.isEmpty // list could be empty
      || s.tail.isEmpty // or singleton
      || !s.zip(s.tail).exists { case (a,b) => a > b }) // sorted
    && s.forall(l contains _) // all elts of sorted list should be in l
    && l.forall(s contains _) // all elts of l should be in sorted list
    )
  }

  // 8.16 grabbed from solutions and needed for 17
  val pint2: Gen[Par[Int]] = choose(-100,100).listOfN(choose(0,20)).map(l => 
    l.foldLeft(Par.unit(0))((p,i) => 
      Par.fork { Par.map2(p, Par.unit(i))(_ + _) }))

  // 8.17
  // val forkProp = Prop.forAllPar(pint2)(i =>
  //   equal(Par.fork(i), i)
  // )
}

case class SGen[+A](forSize: Int => Gen[A]) {
  // 8.11
  def apply(n: Int): Gen[A] = forSize(n)

  def map[B](f: A => B): SGen[B] =
    SGen(forSize(_) map f)

  // def map2[B,C](g: Gen[B])(f: (A,B) => C): Gen[C] =
  //   Gen(sample.map2(g.sample)(f))

  def flatMap[B](f: A => SGen[B]): SGen[B] = 
    SGen(n => 
      forSize(n) flatMap (x => 
        f(x).forSize(n) 
      )
    )
}

