package part4typeclasses

import cats.{Functor, Semigroupal}

object WeakerApplicatives extends App {
  // Cats have a Apply trait with product and ap methods.
  trait MyApply[W[_]] extends Functor[W] with Semigroupal[W] {
    // the product method can be implemented by using the map and ap methods
    override def product[A, B](fa: W[A], fb: W[B]): W[(A, B)] = {
      val functionWrapper: W[B => (A, B)] = map(fa)(a => (b: B) => (a, b))
      ap(functionWrapper)(fb)
    }

    // TODO
    def mapN[A, B, C](tuple: (W[A], W[B]))(f: (A, B) => C): W[C] = {
      val tupleWrapper = product(tuple._1, tuple._2) // wrapper over tuple of A, B: W[(A,B)]
      map(tupleWrapper) { // MyApply extends Functor, so it has access to map method
        case (a, b) => f(a, b) // returns the C from the function f(A, B)
      }
    }

    def ap[B, T](wf: W[B => T])(wa: W[B]): W[T] // fundamental
  }

  trait MyApplicative[W[_]] extends MyApply[W] {
    // the fundamental method of Applicative trait is pure() - wrap a normal value to wrapper type
    // all the other stuff are auxiliary
    def pure[A](x: A): W[A] // fundamental
  }

  // import cats.Applicative // Applicative extends Apply which has ap and product methods

  import cats.Apply
  import cats.instances.option._ // implicit Apply[Option]

  val applyOption = Apply[Option]
  val funcApp = applyOption.ap(Some((x: Int) => x + 1))(Some(2)) // Some(3)
  println(funcApp)
  // the Apply type class and ap method are rarely used by themselves. but the ap method gives rise to the product type
  // which can tuple the wrapper type into a wrapper over tuple automatically, we can wrap more than just 2 elements
  // we can tuple up to 22 elements, and we can apply function of up to 22 elements without the need of unwrap the wrapping types all the time

  import cats.syntax.apply._ // extension methods from Apply

  val tupleOfOptions = (Option(1), Option(2), Option(3))
  val optionOfTuple = tupleOfOptions.tupled // Some(1,2,3) // tupled unwrap the values of all these options, and the values are tupled and then wrapped back. this is by virtue of the presence of the product method
  println(optionOfTuple)
  // this is quite general. if you have tuple of Futures, you can convert it to a tuple containing all 3 values
  // this can be generalized for any type of wrapper type for which you have apply in scope.
  // can be used for Future as well

  // you can also apply a function on those values without needing to unwrap the type all the time
  val sumOption = tupleOfOptions.mapN(_ + _ + _) // Some(6)
  // tupled and mapN are valid for any tuples arity. they can be applied to tuples with up to 22 elements
}
