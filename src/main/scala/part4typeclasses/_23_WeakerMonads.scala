package part4typeclasses

import cats.{Applicative, Apply}

object _23_WeakerMonads extends App {
  // if FlatMap trait extends Apply, then it would be able to implement the fundamental method of Apply (i.e. ap)
  // automatically. so it is natural for FlatMap to extend Apply.
  trait MyFlatMap[M[_]] extends Apply[M] {
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] // fundamental method for FlatMap

    // TODO
    // hint: Apply extends Functor (i.e. the map method). so you have access to map and flatMap (defined above)
    def ap[A, B](wf: M[A => B])(wa: M[A]): M[B] = { // wf: wrapper over a function, wa: wrapper over a value
      // ap is implemented from Apply[]. ap is the fundamental method for Apply
      flatMap(wa)(a => map(wf)(f => f(a)))
      //       |  |         /   \    \/
      //       |  |     M[A=>B] A=>B  B
      //       |  |     \______  ____/
      //     M[A] A=>         M[B]
    }
  }

  // if the MyFlatMap trait extends Apply, then it would normally be able to implement the fundamental method of Apply automatically.
  // so it's natural for FlatMap to extend Apply
  // there is a definitive answer that there is a subtype relationship between FlatMap and Apply

  // Monad extends Applicative with FlatMap

  trait MyMonad[M[_]] extends Applicative[M] with MyFlatMap[M] {
    // Monad's fundamental methods are pure() and flatMap(). pure is fundamental to Applicative. So
    //  MyMonad extends Applicative
    //    def pure[A](value: A): M[A] // once we extends Applicative, pure method is not required

    //    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
    // this flatMap is not fundamental to Monad, but fundamental to another trait called FlatMap trait (MyFlatMap above)

    override def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(x => pure(f(x))) // map should be override if extended from Applicative
  }

  // the above MyMonad definition actually matches with the cats.Monad (which extends Applicative and FlatMap

  import cats.FlatMap
  // The cats FlatMap is rarely used independently because there are other stronger FlatMaps used in practice
  //  - those are monads. HoweveR, FlatMap has its own extension methods
  import cats.syntax.flatMap._
  import cats.syntax.functor._ // map extension method
  // if we import both, we can do for comprehension.

  // given that you have flatMap and functor syntax, you can provide for comprehensions
  def getPairs[M[_] : FlatMap, A, B](numbers: M[A], chars: M[B]): M[(A, B)] = for {
    n <- numbers // needs import cats.syntax.flatMap._
    c <- chars // needs import cats.syntax.functor._
  } yield (n, c) // this is quite similar to what we have defined as getPairs under Monad
  // but here the constraints is not that M has Monad in scope, rather a weaker kind of Monad - FlatMap

  import cats.instances.list._

  println(getPairs[List, Int, String](List(1, 2), List("a", "b"))) // requires implicit from cats.instances.list._
}
