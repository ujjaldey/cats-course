package part4typeclasses

object _21_Applicatives extends App {
  // Applicatives are the extension of Functors (map method) that will introduce the pure method (from monad)
  // Applicative = Functors + the pure method

  // Applicative extends Apply, and Apply extends Functor

  import cats.Applicative
  import cats.instances.list._
  // Applicative comes with the pure method i.e. it has ability to wrap a normal value into a wrapped value

  val listApplicative = Applicative[List] // higher kind of type like Functor or Monad
  val aList = listApplicative.pure(2) // returns the wrapped value, i.e. List(2)
  println(aList)

  import cats.instances.option._ // fetch implicit Applicative[Option]

  val optionApplicative = Applicative[Option]
  val anOption = optionApplicative.pure(2) // returns Some(2)
  println(anOption)

  // Applicatives have the capacity of wrapping a normal value into a wrapped type - Option, List, Future, etc.
  // Applicatives are useful as Functor (they can run map method).
  // plus it has capability of wrapping a normal type into a wrapped type

  // pure extension method

  import cats.syntax.applicative._ // import extension methods
  // then you can have access to the pure method as in implicit extension method given in presence of
  //   an Applicative for that type

  val aSweetList = 2.pure[List] // List(2)
  val aSweetOption = 2.pure[Option] // Some(2)
  println(aSweetList, aSweetOption)
  println("abc".pure[Option])
  println("====")

  // this pure method is same from Monad

  // Monads extend Applicatives (because they inherit the pure method)
  // Applicatives extend Functors
  // (Functors are with map, Applicatives are with pure method, and Monads are with pure and flatMap)

  // Applicatives are rarely used by themselves, because most Applicatives are actually a stronger type (Monda type)
  // Applicatives are rarely used because most of the data structures are monadic values
  // one exception: it is suitable for an Applicatives is Validated
  // Validated does not respect the monadic rules when you chain them.
  // but you can safely wrap a normal value into a Validated and map the Validated instances.

  import cats.data.Validated

  type ErrorsOr[T] = Validated[List[String], T] // List[String] is the error type
  val aValidValue: ErrorsOr[Int] = Validated.valid(43) // same as pure method. returns the wrapper type ErrorsOr[Int]
  val aModifiedValidate: ErrorsOr[Int] = aValidValue.map(_ + 1) // map
  println(aValidValue, aModifiedValidate)

  val validatedApplicative = Applicative[ErrorsOr]
  // the compiler can construct an implicit applicative with my type where the pure method does
  //   what was done above (Validated.valid(43)).
  // and map method does exactly same as the normal map method does on the Validated type

  // the general use case when we can use Applicative independently is for the data types that are Functors
  // (applicable for map) and for those that can wrap a normal value like pure() method.
  // but these data structures are not Monad.

  // Validated is not monad. It's a candidate for Applicative as a strongest type class.

  // TODO: thought experiment
  // Applicative type class already has ap method, so no need to define here
  // B = input type, T = Tuple
  // def ap[W[_], B, T](wf: W[B => T])(wa: W[A]): W[T] = ??? // this is already implemented

  // W = wrapper
  def productWithApplicative[W[_], A, B](wa: W[A], wb: W[B])(implicit applicative: Applicative[W]): W[(A, B)] = {
    val functionWrapper: W[B => (A, B)] = applicative.map(wa)(a => (b: B) => (a, b))
    applicative.ap(functionWrapper)(wb)
    // functionWrapper converts A to (A,B) (B and T in above definition of ap)
    // wb is a wrapper of B
  }

  // Applicatives have this ap[W[_], B, T](wf: W[B => T])(wa: W[B]): W[T]
  // Applicatives can implement product from Semigroupal (in presence of ap method)
  // Applicatives extends Semigroupal (i.e. it can define a product method in presence of this ap method)
}
