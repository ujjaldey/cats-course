package part4typeclasses

import cats.{Applicative, Apply}

object WeakerMonads extends App {
  trait MyFlatMap[M[_]] extends Apply[M] {
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    // TODO
    // hint: Apply extends Functor
    def ap[A, B](wf: M[A => B])(wa: M[A]): M[B] = { // ap is implemented from Apply[]
      flatMap(wa)(a => map(wf)(f => f(a)))
      //       |  |         /   \    \/
      //       |  |     M[A=>B] A=>B  B
      //       |  |     \______  ____/
      //       M[A] A=>        M[B]
    }
  }

  // if the MyFlatMap trait extends Apply, then it would normally be able to implement the fundamental method of Apply automatically.
  // so it's natural for FlatMap to extend Apply
  // there is a definitive answer that there is a subtype relationship between FlatMap and Apply

  trait MyMonad[M[_]] extends Applicative[M] with MyFlatMap[M] {
    // Monad's fundamental methods are pure() and flatMap(). pure is fundamental to Applicative
    //    def pure[A](value: A): M[A] // once we extends Applicative, pure method is not required

    //    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] // this flatMap is not fundamental to Monad, but fundamental to MyFlatMap trait

    override def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(x => pure(f(x))) // map should be override if extended from Applicative
  }

  // the above MyMonad definition actually matches with the cats.Monad (which extends Applicative and FlatMap

  import cats.FlatMap // The cats FlatMap is rarely used independently because there are other stronger FlatMaps used in practice - those are monads.
  // However FlatMap has its own extension methods
  import cats.syntax.flatMap._
  import cats.syntax.functor._ // map extension method

  def getPairs[M[_] : FlatMap, A, B](numbers: M[A], chars: M[B]): M[(A, B)] = for {
    a <- numbers
    c <- chars
  } yield (a, c)
}
