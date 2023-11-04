package playground

import cats.implicits.catsStdInstancesForFuture
import cats.{Apply, Semigroupal}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Misc1 extends App {
  Future(1).flatMap(x => Future(41).map(y => x + y)).foreach(println)

  (for {
    x <- Future(1)
    y <- Future(41)
  } yield (x + y)).foreach(println)

  val apply1 = Apply[Future].map2(Future(1), Future(41))(_ + _)
  apply1.foreach(println)

  val sgroupal1 = Semigroupal[Future].product(Future(1), Future(41))
  sgroupal1.foreach(println)

  trait MySemigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }
}
