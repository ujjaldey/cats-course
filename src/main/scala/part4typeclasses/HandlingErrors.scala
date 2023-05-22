package part4typeclasses

import cats.{Applicative, ApplicativeError, Monad}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object HandlingErrors extends App {
  trait MyApplicativeError[M[_], E] extends Applicative[M] {
    // pure method from Applicative
    def raiseError[A](e: E): M[A]

    def handleErrorWith[A](ma: M[A])(func: E => M[A]): M[A]

    def handleError[A](ma: M[A])(func: E => A): M[A] = handleErrorWith(ma)(e => pure(func(e)))
  }

  // M is higher-kinded type, E is error type. E is not a real JVM exception type, E can be anything that you want to consider as error
  trait MyMonadError[M[_], E] extends MyApplicativeError[M, E] with Monad[M] {
    // A: value type, E: error type
    //    def raiseError[A](e: E): M[A] // move this under ApplicativeError, as it's not fundamental method for MonadError
    // ensure is fundamental method for MonadError
    def ensure[A](ma: M[A])(error: E)(predicate: A => Boolean): M[A] // M will encapsulate the error E depending on the implementation
  }

  import cats.MonadError
  import cats.instances.either._ // impilct MonadError

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

  // recoverWith - to another error type
  val handledErrorWith: Either[String, Int] = monadErrorEither.handleErrorWith(failure) {
    case "Badness" => monadErrorEither.pure(44) // ErrorOr[Int]
    case _ => Left("something else") // ErrorOr[Int], yes Int, as ErrorOr[A] is of type Either[String, A]
  }

  println(handledError, handledErrorWith)


  // filter - predicate in 3rd argument will check if value in 1st argument is true, then it will turn to a failure (Left) with 2nd argument wrapped
  val filteredSuccess = monadErrorEither.ensure(success)("Number too small")(_ > 100) // rarely used, mostly used as extension method

  // Try and Future

  import cats.instances.try_._ // implicit MonadError[Try], E = Throwable

  val exception = new RuntimeException("Really bad")
  val pureException: Try[Int] = MonadError[Try, Throwable].raiseError(exception) // not throwing the exception, but rather storing it in FP way with raiseError() method. return Try[Nothing]
  println(pureException)

  import cats.instances.future._

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val pureException2 = MonadError[Future, Throwable].raiseError(exception) // Future which will complete with a Failure(exception)
  println(pureException2)
  println("====")

  // applicatives => ApplicativeError. Validated Applicative instance is not Monad

  import cats.data.Validated
  import cats.instances.list._ // implicit Semigroup[List] => ApplicativeError[ErrorsOr, List[String]]

  type ErrorsOr[T] = Validated[List[String], T]
  val applErrorVal = ApplicativeError[ErrorsOr, List[String]] // requires cats.instances.list._
  // ApplicativeError has same api as MonadError, so you can have the pure() method.
  // MonadError extends Monad, similarly ApplicativeError extends Applicative
  // you also have access to pure, raiseError, handleError, handleErrorWith

  // extension methods

  import cats.syntax.applicative._
  import cats.syntax.applicativeError._ // import raiseError, handleError, handleErrorWith

  val extendedSuccess: ErrorsOr[Int] = 42.pure[ErrorsOr] // requires implicit ApplicativeError[ErrorsOr, List[String]]
  val extendedError: ErrorsOr[Int] = List("Badness").raiseError[ErrorsOr, Int]
  println(extendedSuccess, extendedError)
  val recoveredError: ErrorsOr[Int] = extendedError.recover {
    case _ => 43
  }
  println(recoveredError)

  import cats.syntax.monadError._ // ensure

  val testedSuccess = success.ensure("Something bad")(_ > 100)
  println(testedSuccess)
  println("====")

  // MonadError extends ApplicativeError, in the same style, Monad extends Applicative
  // hence we can add the extends and with in the above definition of trait MyMonadError[M[_], E]
  // actual cats MonadError also has the same structure
}
