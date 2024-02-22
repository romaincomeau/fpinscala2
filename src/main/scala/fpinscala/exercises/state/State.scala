package fpinscala.exercises.state

trait RNG:
  def nextInt: (Int, RNG) /* Should generate a random `Int`. We'll later define
   * other functions in terms of `nextInt`. */

object RNG:
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL /* `&` is
       * bitwise AND. We use the current seed to generate a new seed. */
      val nextRNG = Simple(newSeed) /* The next state, which is an `RNG`
       * instance created from the new seed. */
      val n       = (newSeed >>> 16).toInt /* `>>>` is right binary shift with zero
       * fill. The value `n` is our new pseudo-random integer. */
      (n, nextRNG) /* The return value is a tuple containing both a
       * pseudo-random integer and the next `RNG` state. */

  type Rand[+A] = RNG => (A, RNG)
  extension [A](ra: Rand[A])
    def flatMap[B](f: A => Rand[B]): Rand[B] = rng0 =>
      val (a, rng1) = ra(rng0)
      f(a)(rng1)

    def map[B](f: A => B): Rand[B] = rng0 =>
      val (a, rng1) = ra(rng0)
      (f(a), rng1)

  val int: Rand[Int] = _.nextInt

  val two: Rand[(Int, Int)] = for
    x <- int
    y <- int
  yield (x, y)

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def boolean: Rand[Boolean] =
    map(nonNegativeLessThan(2))(x => if x == 1 then true else false)

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (x, r) = rng.nextInt
    (if x < 0 then -(x + 1) else x, r)

  def double(rng: RNG): (Double, RNG) =
    val (x, r) = nonNegativeInt(rng)
    (x / (Int.MaxValue.toDouble + 1), r)

  val doubleRand: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue + 1))

  def intDouble(rng: RNG): ((Int, Double), RNG) =
    val (x, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((x, d), r2)

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    val ((x, d), r1) = intDouble(rng)
    ((d, x), r1)

  def double3(rng: RNG): ((Double, Double, Double), RNG) =
    val (x, r1) = double(rng)
    val (y, r2) = double(r1)
    val (z, r3) = double(r2)
    ((x, y, z), r3)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng0 =>
      val (a, rng1) = ra(rng0)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs.foldRight(unit[List[A]](Nil))((a, acc) => map2(a, acc)(_ :: _))

  def ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt): i =>
      val mod = i % n
      if i + (n - 1) - mod >= 0 then unit(mod) else nonNegativeLessThan(n)

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] =
    flatMap(r)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(
      f: (A, B) => C
  ): Rand[C] =
    flatMap(ra)(a => mapViaFlatMap(rb)(b => f(a, b)))

opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      s0 =>
        val (a, s1) = underlying(s0)
        f(a).run(s1)

    def map[B](f: A => B): State[S, B] =
      flatMap(f andThen unit)

    def map2[B, C](s2: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap(a => s2.map(b => f(a, b)))

  def apply[S, A](f: S => (A, S)): State[S, A] = f

  def unit[S, A](a: A): State[S, A] = s => (a, s)

  def get[S]: State[S, S] = s => (s, s)

  def set[S](s: S): State[S, Unit] = _ => ((), s)

  def modify[S](f: S => S): State[S, Unit] = for
    s       <- get
    _: Unit <- set(f(s))
  yield ()

  private def sequence_1[S, A](actions: List[State[S, A]]): State[S, List[A]] =
    actions.foldRight(unit[S, List[A]](Nil)): (f, acc) =>
      f.map2(acc)(_ :: _)

  def traverse[S, A, B](as: List[A])(f: A => State[S, B]): State[S, List[B]] =
    as.foldRight(unit[S, List[B]](Nil))((a, acc) => f(a).map2(acc)(_ :: _))

  def sequence[S, A](actions: List[State[S, A]]): State[S, List[A]] =
    traverse(actions)(a => a)

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:
  val update = (i: Input) =>
    (s: Machine) =>
      (i, s) match
        case (_, Machine(_, 0, _))              => s
        case (Input.Coin, Machine(false, _, _)) => s
        case (Input.Turn, Machine(true, _, _))  => s
        case (Input.Coin, Machine(true, candy, coin)) =>
          Machine(false, candy, coin + 1)
        case (Input.Turn, Machine(false, candy, coin)) =>
          Machine(true, candy - 1, coin)

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for
      x <- State.traverse(inputs)(i => State.modify(update(i)))
      s <- State.get
    yield (s.coins, s.candies)
