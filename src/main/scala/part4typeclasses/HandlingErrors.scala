package part4typeclasses

import cats.{Applicative, Monad}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object HandlingErrors extends App {
  trait MyApplicativeError[M[_], E] extends Applicative[M] {
    // pure from Applicative

    // raiseError is not really a fundamental method of MonadError, it's a fundamental method from ApplicativeError. M[A] will somehow incorporate the E error type. E is not a real JVM exception type, E can be anything that you want to consider as error (general representation of error)
    def raiseError[A](e: E): M[A]

    def handleErrorWith[A](ma: M[A])(func: E => M[A]): M[A] // fundamental method, as handleError is defined in terms of handleErrorWith

    // A is the value type. M[A] is the wrapper on A
    def handleError[A](ma: M[A])(func: E => A): M[A] = handleErrorWith(ma)(e => pure(func(e)))
  }

  // M is higher-kinded type, E is error type. E is not a real JVM exception type, E can be anything that you want to consider as error
  // Cats MonadError also has similar signature. it extends ApplicativeError with Monad
  trait MyMonadError[M[_], E] extends MyApplicativeError[M, E] with Monad[M] {
    // A: value type, E: error type
    //    def raiseError[A](e: E): M[A] // move this under ApplicativeError, as it's not fundamental method for MonadError
    // ensure is fundamental method for MonadError
    def ensure[A](ma: M[A])(error: E)(predicate: A => Boolean): M[A] // M will encapsulate the error E depending on the implementation
  }

  import cats.MonadError
  import cats.instances.either._ // implicit MonadError

  type ErrorOr[A] = Either[String, A]
  val monadErrorEither = MonadError[ErrorOr, String] // error type should be identical to the error type in Either (i.e. String)
  // MonadError is also a Monad, so it has access to fundamental pure, flatMap, and other methods that Monad has
  val success = monadErrorEither.pure(32) // Either[String, Int] = Right(32)
  val failure = monadErrorEither.raiseError[Int]("something wrong") // Either[String, Int] = Left("something wrong")
  println(success, failure)

  // recover - to a data type
  val handledError: Either[String, Int] = monadErrorEither.handleError(failure) {
    case "Badness" => 44
    case _ => 89
  }

  // or
  val handledError2: ErrorOr[Int] = monadErrorEither.handleError(failure) {
    case "Badness" => 44
    case _ => 89
  }

  // recoverWith - to another error type (instead of the value type)
  val handledErrorWith: Either[String, Int] = monadErrorEither.handleErrorWith(failure) {
    case "Badness" => monadErrorEither.pure(44) // ErrorOr[Int]
    case _ => Left("something else") // ErrorOr[Int], yes Int, as ErrorOr[A] is of type Either[String, A]
  }
  // or
  val handledErrorWith2: ErrorOr[Int] = monadErrorEither.handleErrorWith(failure) {
    case "Badness" => monadErrorEither.pure(44) // ErrorOr[Int]
    case _ => Left("something else") // ErrorOr[Int], yes Int, as ErrorOr[A] is of type Either[String, A]
  }

  println(handledError, handledError2, handledErrorWith, handledErrorWith2)

  // filter - predicate in 3rd argument will check if value in 1st argument is true, then it will turn to a failure (Left) with 2nd argument wrapped
  val filteredSuccess = monadErrorEither.ensure(success)("Number too small")(_ > 100) // rarely used, mostly used as extension method
  println(filteredSuccess)
  println("====")

  // Try and Future

  import cats.instances.try_._ // implicit MonadError[Try], E = Throwable

  val exception = new RuntimeException("Really bad")
  val pureException: Try[Int] = MonadError[Try, Throwable].raiseError(exception) // not throwing the exception, but rather storing it in FP way with raiseError() method. return Try[Nothing], so it can be cast with anything e.g. Try[Int]
  println(pureException)

  import cats.instances.future._

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val pureException2 = MonadError[Future, Throwable].raiseError(exception) // Future which will complete with a Failure(exception)
  println(pureException2)
  println("====")

  // applicatives => ApplicativeError.
  // Validated Applicative instance is not Monad. There are not too many instances of Applicative that are not Monad

  import cats.ApplicativeError
  import cats.data.Validated
  import cats.instances.list._

  type ErrorsOr[T] = Validated[List[String], T]
  val applErrorVal = ApplicativeError[ErrorsOr, List[String]] // requires cats.instances.list._

  // ApplicativeError has same api as MonadError, so you can have the pure() method.
  // MonadError extends Monad, similarly ApplicativeError extends Applicative
  // you also have access to pure (from applicative), raiseError (abstract), handleError (defined in terms of handleErrorWith), handleErrorWith (abstract)

  // extension methods

  // imports pure

  import cats.syntax.applicative._
  import cats.syntax.applicativeError._ // imports raiseError, handleError, handleErrorWith

  val extendedSuccess: ErrorsOr[Int] = 42.pure[ErrorsOr] // requires implicit ApplicativeError[ErrorsOr, List[String]]
  val extendedError: ErrorsOr[Int] = List("Badness").raiseError[ErrorsOr, Int]
  println(extendedSuccess, extendedError)

  val recoveredError: ErrorsOr[Int] = extendedError.recover {
    case _ => 43
  }
  println(recoveredError)

  import cats.syntax.monadError._ // ensure extension method

  val testedSuccess = success.ensure("Something bad")(_ > 100)
  println(testedSuccess)
  println("====")

  // MonadError extends ApplicativeError, in the same style, Monad extends Applicative
  // hence we can add the extends and with in the above definition of trait MyMonadError[M[_], E]
  // actual cats MonadError also has the same structure
}
