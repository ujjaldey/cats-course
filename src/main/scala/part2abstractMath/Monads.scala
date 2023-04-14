package part2abstractMath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Monads extends App {
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
  println(numberFuture.flatMap(n => charFuture.map(c => (n, c))))
  println(for {
    n <- numberFuture
    c <- charFuture
  } yield (n, c))

  println("===")
  // same way, use Try

  /*
    Pattern
    - wrapping a value into a monadic (M) value
    - the flatMap mechanism (flatMap guarantees a sequential order of execution)

    the cats type class that formalizes these 2 capabilities is called: MONDAS
   */

  trait MyMonad[M[_]] {
    def pure[A](value: A): M[A]

    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] // not A=>B, rather A=>M[B]
  }
  // Cats Monad

  import cats.Monad
  import cats.instances.option._ // implicit Monad[Option]

  val optionMonad = Monad[Option]
  val anOption = optionMonad.pure(4) // Option(4) == Some(4)
  val aTransformedOption = optionMonad.flatMap(anOption)(x => if (x % 3 == 0) Some(x + 1) else None)
  println(aTransformedOption)

  import cats.instances.list._ // implicit Monad[List]

  val listMonad = Monad[List]
  val aList = listMonad.pure(3) // List(3)
  val aTransformedList = listMonad.flatMap(aList)(x => List(x, x + 1)) // List(4,5)

  // TODO 2: use a Monad[Future]

  import cats.instances.future._

  val futureMonad = Monad[Future] // requires an implicit ExecutionContext - which we have defined above already
  val aFuture = futureMonad.pure(3)
  val aTransformedFuture = futureMonad.flatMap(aFuture)(x => Future(x * 3)) // future that will end up with a Success(9)
  println(aTransformedFuture)

  // specialized API
  def getPairsList(numbers: List[Int], chars: List[Char]): List[(Int, Char)] = numbers.flatMap(n => chars.map(c => (n, c)))

  // in case you want to have same feature for Option or Future, repeat the code:
  def getPairsOption(numbers: Option[Int], chars: Option[Char]): Option[(Int, Char)] = numbers.flatMap(n => chars.map(c => (n, c)))

  def getPairsFuture(numbers: Future[Int], chars: Future[Char]): Future[(Int, Char)] = numbers.flatMap(n => chars.map(c => (n, c)))

  // generalize
  def getPairs[M[_], A, B](ma: M[A], mb: M[B])(implicit monad: Monad[M]): M[(A, B)] =
    monad.flatMap(ma)(a => monad.map(mb)(b => ((a, b))))

  val numbersOption = Option(1)
  val charsOption = Option('a')
  val numbersFuture = Future(1)
  val charsFuture = Future('a')

  println(getPairs(numbersList, charsList))
  println(getPairs(numbersOption, charsOption))
  getPairs(numbersFuture, charsFuture).foreach(println)
}
