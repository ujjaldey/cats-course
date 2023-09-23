package part2abstractMath

import java.util.concurrent.Executors
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Try

object _11_Monads extends App {
  // lists
  val numbersList = List(1, 2, 3)
  val charsList = List('a', 'b', 'c')

  // TODO 1.1: how do you create all combinations of (number, char)?
  println(numbersList.flatMap(n => charsList.map(c => (n, c))))
  println(for {
    n <- numbersList
    c <- charsList
  } yield (n, c)) // identical - for comprehension is collapsed to flatMaps and map by the compiler

  val numberOption = Option(2)
  val charOption = Option('d')
  // TODO 1.2: how do you create the combination of (number, char)?
  println(numberOption.flatMap(n => charOption.map(c => (n, c))))
  println(for {
    n <- numberOption
    c <- charOption
  } yield (n, c))

  // futures
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val numberFuture = Future(42)
  val charFuture = Future('z')

  // TODO 1.3: how do you create the combination of (number, char)?
  val x1 = numberFuture.flatMap(n => charFuture.map(c => (n, c)))
  val x2 = for {
    n <- numberFuture
    c <- charFuture
  } yield (n, c)

  println(Await.result(x1, Duration.Inf))
  println(Await.result(x2, Duration.Inf))
  println("===")

  // same way, use Try

  /*
    Pattern
    - wrapping a value into a monadic (M) value (like List, Option, Future)
    - ability to transform those values using a flatMap. the flatMap mechanism (flatMap guarantees a sequential order of execution)

    the cats type class that formalizes these 2 capabilities is called: MONADS
   */

  // higher kinded type class
  trait MyMonad[M[_]] {
    def pure[A](value: A): M[A] // takes A and returns M[A]. i.e. say, Int to List[Int]
    // pure is a very general method. It takes a value, and wraps that value into M of that value

    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] // not A=>B, rather A=>M[B]
  }

  // Cats Monad

  import cats.Monad
  import cats.instances.option._ // implicit Monad[Option] as we have cats.Monad in scope

  val optionMonad = Monad[Option]
  val anOption = optionMonad.pure(4) // Option(4) == Some(4)
  val aTransformedOption = optionMonad.flatMap(anOption)(x => if (x % 3 == 0) Some(x + 1) else None)
  val aTransformedOption2 = optionMonad.flatMap(anOption)(x => if (x % 3 != 0) Some(x + 1) else None)
  println(anOption, aTransformedOption, aTransformedOption2)

  import cats.instances.list._ // will bring implicit Monad[List] in scope

  val listMonad = Monad[List]
  val aList = listMonad.pure(3) // List(3)
  val aTransformedList = listMonad.flatMap(aList)(x => List(x, x + 1)) // List(3,4)
  println(aList, aTransformedList)

  // TODO 2: use a Monad[Future]

  import cats.instances.future._ // will bring implicit Monad[Future] in scope

  val futureMonad = Monad[Future] // requires an implicit ExecutionContext - which we have defined above already
  val aFuture = futureMonad.pure(3)
  val aTransformedFuture = futureMonad.flatMap(aFuture)(x => Future(x * 3)) // future that will end up with a Success(9)
  println(Await.result(aTransformedFuture, Duration.Inf))

  // why this API with this complex flatMap useful (since all standard collection like List, Option already have the flatMap)
  //  - because it's useful for general APIs

  // specialized API
  def getPairsList(numbers: List[Int], chars: List[Char]): List[(Int, Char)] = numbers.flatMap(n => chars.map(c => (n, c)))

  // in case you want to have same feature for Option or Future, repeat the code:
  def getPairsOption(number: Option[Int], char: Option[Char]): Option[(Int, Char)] = number.flatMap(n => char.map(c => (n, c)))

  def getPairsFuture(number: Future[Int], char: Future[Char]): Future[(Int, Char)] = number.flatMap(n => char.map(c => (n, c)))

  // generalize
  // M[_] is a higher kinded type
  // if we have Monad[M] in scope, we can do the similar thing in a generic way
  def getPairs[M[_], A, B](ma: M[A], mb: M[B])(implicit monad: Monad[M]): M[(A, B)] = {
    monad.flatMap(ma)(a => monad.map(mb)(b => ((a, b)))) // monad supports flatMap and map by default
    // we can flatMap any kind of data structure (List, Option, Future) as long as we have an implicit Monad in scope
  }

  val numbersOption = Option(1)
  val charsOption = Option('a')
  val numbersFuture = Future(1)
  val charsFuture = Future('a')

  println(getPairs(numbersList, charsList))
  println(getPairs(numbersOption, charsOption))
  getPairs(numbersFuture, charsFuture).foreach(println) // if we just print the Future, we don't see anything

  // we can transform any kind of monadic value as long as we have an implicit monad in scope

  println("===")
  println("===")
  println("===")

  //  PART 2

  // extension methods - weirder imports - pure, flatMap

  import cats.syntax.applicative._ // pure is here

  // cats.syntax.applicative._ imports the pure() extension method
  // pure requires an implicit Applicative[F]. Monads are a specialized type Applicative and the Monad will be
  //  brought in scope if we type the right type (say Option or List)
  val oneOption = 1.pure[Option] // implicit Monad[Option] will be used => Some(1)
  val oneList = 1.pure[List] // List(1) // flatMap is here
  println("a".pure[List])
  println(oneOption, oneList)

  // the pure() method is part of the Applicative type.

  import cats.implicits.catsStdInstancesForTry

  println("abc".pure[Try])

  import cats.syntax.flatMap._ // flatMap is here

  val oneOptionTransformed = oneOption.flatMap(x => (x + 1).pure[Option])
  println(oneOption, oneOptionTransformed)

  // TODO 3: implement the map method in MyMonad
  // Monads are also Functor. Monads extends Functors
  val oneOptionMapped = Monad[Option].map(Option(2))(_ + 1)
  println(oneOptionMapped) // map is here

  import cats.syntax.functor._ // map is here. this also has the extension methods for Monad

  trait MyMonadNew[M[_]] {
    def pure[A](value: A): M[A]

    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] // not A=>B, rather A=>M[B]

    // TODO implement this
    def map[A, B](ma: M[A])(f: A => B): M[B] = // map has A=>B, not A=>M[B]
      flatMap(ma)(x => pure(f(x))) // f(x) returns a B, but we need M[B]. Hence we use the pure() method to covert B to M[B]

    // cats monad's map is also implemented the same way. so the only abstract methods that Mondas have are pure() and flatMap()
    // so we can say that Monad, by default, extends a Functor because Monad can provide the fundamental method of Functor i.e. a map().
    // If you see the definition of Monad, it extends Applicative[F] which extends Apply[F] which extends Functor[F]
    // MONADS ARE ALSO FUNCTORS. Monads also have access to Functor's extension methods
  }

  val oneOptionMapped2 = oneOption.map(_ + 2)
  println(oneOptionMapped2)

  // as Monad has access to map() and flatMap(), it can also have access to for comprehensions
  // for comprehensions:
  val composedOptionFor = for {
    one <- 1.pure[Option]
    two <- 2.pure[Option]
  } yield one + two

  val composedOptionFor2 = 1.pure[Option].flatMap(one => 2.pure[Option].map(two => one + two))

  println("==>", composedOptionFor)
  println("==>", composedOptionFor2)

  println("====")

  // TODO 4: implement a shorter version of getPairs using for-comprehensions
  //  def getPairsFor[M[_], A, B](ma: M[A], mb: M[B])(implicit monad: Monad[M]): M[(A, B)] = {
  // or remove the implicit and rewrite as:
  def getPairsFor[M[_] : Monad, A, B](ma: M[A], mb: M[B]): M[(A, B)] = { // implicit Monad[M] can be short-formed as M[_] : Monad
    //    ma.flatMap(a => mb.map(b=> (a, b)))
    // or same as above:
    for {
      a <- ma
      b <- mb
    } yield (a, b)
  }

  println(getPairsFor(numbersList, charsList))
  println(getPairsFor(numbersOption, charsOption))
  getPairsFor(numbersFuture, charsFuture).foreach(println)
}
