package part4typeclasses

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object _20_Semigroupals extends App {
  // Semigroups are type classes with a single method that will allow you to return tuple values
  // regardless of how they are computed

  trait MySemigroupal[F[_]] { // F is higher order type e.g. List
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] // fundamental method of Semigroupal - you tuple the values
  }

  // the purpose of Semigroupal is to tuple the values by unwrapping them and wrapping them back
  // same as the case of combine in Semigroup

  import cats.Semigroupal
  import cats.instances.option._ // imports implicit Semigroupal[Option]

  val optionSemigroupal = Semigroupal[Option] // uses implicit Semigroupal[Option] from cats.instances.option._
  val aTupledOption = optionSemigroupal.product(Some(123), Some("a string")) // Some(123, "a string")
  val aNoneTupled = optionSemigroupal.product(Some(123), None)
  // None - if any one item is None, the result will be None

  // the product() will simply tuple the values regardless of the wrapper type
  // which will be unwrapped by the implicit type class instance

  println(aTupledOption)
  println(aNoneTupled)

  import cats.instances.future._ // implicit Semigroupal[Future]

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  val aTupledFuture = Semigroupal[Future].product(Future("the meaning of life"), Future(42))
  // these 2 Futures will run in parallel, and once they both return values,
  // the value will be tupled together in a new Future: Future("the meaning of life", 42)
  aTupledFuture.foreach(println)

  import cats.instances.list._ // implicit Semigroupal[List]
  // here we actually imports Monad[List] which is also a Semigroupal[List]

  val aTupledList = Semigroupal[List].product(List(1, 2), List("a", "b"))
  // returns the List of the cartesian product of 1,2 and a,b (instead of zipping the result)
  println(aTupledList)

  // TODO: implement product with monad

  import cats.Monad
  // extension methods for map and flatMap applicable to monad
  import cats.syntax.flatMap._
  import cats.syntax.functor._ // for map

  // the fundamental methods for monads are pure, map, and flatMap
  def productWithMonads[F[_], A, B](fa: F[A], fb: F[B])(implicit monad: Monad[F]): F[(A, B)] =
    monad.flatMap(fa)(a => monad.map(fb)(b => (a, b)))

  def productWithMonadsFor[F[_], A, B](fa: F[A], fb: F[B])(implicit monad: Monad[F]): F[(A, B)] =
    for {
      a <- fa
      b <- fb
    } yield (a, b)

  println(productWithMonads(List(1, 2), List("c", "d")))
  println(productWithMonadsFor(List(1, 2), List("e", "f")))

  // from Monads.scala
  // as we can safely define the product method from map and flatMap,
  // we can say MyMonad can safely extend MySemigroupal
  trait MyMonad[M[_]] extends MySemigroupal[M] {
    def pure[A](value: A): M[A]

    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(x => pure(f(x)))

    // define new method
    def product[A, B](fa: M[A], fb: M[B]): M[(A, B)] = flatMap(fa)(a => map(fb)(b => (a, b)))
  }

  // MONADS EXTEND SEMIGROUPALS
  // this explains why the product returns a cartesian product - when we import cats.instances.list._,
  // we actually import Monad[List] which is also a Semigroupal[List].
  // and because Monad[List] will run product in terms of for comprehension,
  // this is why we obtain the cartesian product.

  // why semigroupals are important when monads can do cartesian product just fine -
  // monad product follow a for comprehension which is a sequence of map and flatMap
  // and maps/flatMaps obey the so-called monad laws.
  // these monad laws are there to impose the sequencing of operations, whereas we might want to
  // combine value without the need of imposing sequencing of evaluation.

  println("====")

  // Validated is a good use case for Semigroupals
  // Validated can be used to combining instances of type Validated without needing to follow the Monad laws

  import cats.data.Validated

  type ErrorsOr[T] = Validated[List[String], T]
  val validatedSemigroupal = Semigroupal[ErrorsOr]
  // requires the implicit Semigroup[List[_]] which we can provide with import cats.instances.list._ (provided above)

  // and we are able to combine the instances of Validated with the product method of the Semigroupal
  // without needing to follow the sequencing of monad laws
  val invalidsCombination = validatedSemigroupal.product(
    Validated.invalid(List("something wrong", "something else wrong")),
    Validated.invalid(List("this can't be right"))
  )

  // all different lists get combined into one list
  // so Validated would combine through the combination function of the error type
  println(invalidsCombination)

  // you won't get the same result using the monadic type like Either
  type EitherErrorsOr[T] = Either[List[String], T]

  import cats.instances.either._ // implicit Monad[Either]

  val eitherSemigroupal = Semigroupal[EitherErrorsOr] // Semigroupal of type EitherErrorsOr in scope

  val eitherCombination = eitherSemigroupal.product(
    // Semigroupal[EitherErrorsOr] will be the implicit Monad[EitherErrorOr]. so the product method will be
    // implemented in terms of map and flatMap
    Left(List("something wrong", "something else wrong")),
    Left(List("this can't be right"))
  )
  // as the Eithers are of type Left, the last error "this can't be right" will not be propagated,
  // because the flatMap method on Either short circuits the evaluation of the 2nd Either.
  println(eitherCombination)
  // so we cannot use Monadic type like Either for managing errors, it will lose track of the errors.
  // so Validated and Semigroupals are most useful in this case without necessarily needing to follow monad laws
  // -- here we are referring to the associativity law of monad
  // associativity: m.flatMap(f).flatMap(g) == m.flatMap(x=>f(x).flatMap(g)
  //    -- chaining 2 flatMaps are same as calling a single flatMap and then applying another flatMap
  // this is true for Either, but not true for Validated. and yet Validated is more useful in this case.

  // TODO 2: define a Semigroupal[List] which does a zip (instead of cartesian product)
  val zipListSemigroual: Semigroupal[List] = new Semigroupal[List] {
    override def product[A, B](listA: List[A], listB: List[B]): List[(A, B)] = listA.zip(listB)
  }
  println(zipListSemigroual.product(List(3, 4), List("x", "y")))

  // don't get confuse with Semigroup and Semigroupal
  // Semigroup has combine method |+|
  // Semigroupal has product method
}
