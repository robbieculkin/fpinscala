package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def boolean(rng: RNG): (Boolean, RNG) =
    rng.nextInt match { case (i,rng2) => (i%2==0,rng2) }

  // maps a function f to the output of random A generator s
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  //6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, nrng) = rng.nextInt
    (if (i < 0) -(i + 1) else i, nrng)
  }

  //6.2
  def double(rng: RNG): (Double, RNG) =  {
    val (i, nrng) = nonNegativeInt(rng)
    (i.toDouble/(Int.MaxValue.toDouble + 1), nrng)
  }

  //6.5
  val doubleViaMap:Rand[Double] = 
    map(nonNegativeInt)(i => i.toDouble/(Int.MaxValue.toDouble + 1))

  //6.3
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng2) = nonNegativeInt(rng)
    val (d, rng3) = nonNegativeInt(rng2)
    ((i,d),rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i,d),rng2) = intDouble(rng)
    ((d,i),rng2) 
  }
  // pretty sure the authors made this so I'd think it was repetitive
  // neato solution likely to follow
  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng2) = nonNegativeInt(rng)
    val (d2, rng3) = nonNegativeInt(rng2)
    val (d3, rng4) = nonNegativeInt(rng3)
    ((d1,d2,d3), rng4)
  }

  //6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count > 0) {
      val (i,rng2) = rng.nextInt
      val (l, _) = ints(count-1)(rng2)
      (i::l, rng)
    }
    else (List[Int](),rng)
  }

  //6.6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a,b), rng3)
    }

  //6.7
  // have to use unit(List[A]()) to mark the end of the list 
  // and tell the compiler what type of list foldRight will return
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = 
    fs.foldRight(unit(List[A]()))((a,b) => map2(a,b)(_::_))

  def intsViaSequence(count: Int): Rand[List[Int]] = 
    sequence(List.fill(count)(int))

  //6.8
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) 
        unit(mod)
      else nonNegativeLessThan(n)
    }

  //6.9
  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s){a => unit(f(a))}

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
    flatMap(ra)(a => map(rb)(b => f(a, b)))
}

//6.10
case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a=> State.unit(f(a)))
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a=> sb.map(b=>f(a,b)))
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s2) = run(s)
    f(a).run(s2)
  })
}

object State {
  //needed for map in 6.10
  def unit[S,A](a:A): State[S,A] =
    State(s => (a, s))
  //6.10
  def sequence[S,A](l: List[State[S, A]]): State[S, List[A]] =
    l.foldRight(unit[S, List[A]](List[A]()))((a,b) => a.map2(b)(_::_))

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

// object Candy {
//   def update = (i: Input) => (s: Machine) => 
//     (i,s) match {
//       case (_, Machine(_, 0, _)) => s // no candy left
//       case (Coin, Machine(_, cand, coin)) => Machine(false, cand, coin+1)
//       case (Turn, Machine(false,cand,coin)) => Machine(true, cand-1,coin)
//       case (Turn, Machine(true,_,_)) => s
//     }

//   def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = 
//     for {
//       _ <- sequence(inputs map (modify[Machine] _ compose update))
//       s <- get
//     } yield (s.candies, s.coins)

// }
