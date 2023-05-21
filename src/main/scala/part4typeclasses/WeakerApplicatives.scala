package part4typeclasses

import cats.{Functor, Semigroupal}

object WeakerApplicatives extends App {
  // Cats have a Apply trait with product and ap methods.
  trait MyApply[W[_]] extends Functor[W] with Semigroupal[W] {
    // the product method can be implemented by using the map and ap methods
    override def product[A, B](fa: W[A], fb: W[B]) = {
      val functionWrapper: W[B => (A, B)] = map(fa)(a => (b: B) => (a, b))
      ap(functionWrapper)(fb)
    }

    // TODO
    def mapN[A, B, C](tuple: (W[A], W[B]))(f: (A, B) => C): W[C] = {
      val tupleWrapper = product(tuple._1, tuple._2)
      map(tupleWrapper) {
        case (a, b) => f(a, b)
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
  import cats.instances.option._ // implict Apply[Option]

  val applyOption = Apply[Option]
  val funcApp = applyOption.ap(Some((x: Int) => x + 1))(Some(2)) // Some(3)
  println(funcApp)

  import cats.syntax.apply._ // extension methods from Apply

  val tupleOptions = (Option(1), Option(2), Option(3))
  val optionOfTuple = tupleOptions.tupled // Some(1,2,3)
  println(optionOfTuple)
  // this is quite general. if you have tuple of Futures, you can convert it to a tuple containing all 3 values
  // this can be generalized for any type of wrapper type for which you have apply in scope.

  // you can also apply a function on those values without needing to unwrap the type all the time
  val sumOption = tupleOptions.mapN(_ + _ + _) // Some(6)
  // this can be applied to tuples with up to 22 elements

}
