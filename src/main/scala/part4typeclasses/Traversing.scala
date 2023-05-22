package part4typeclasses

import cats.Applicative

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Traversing extends App {
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val servers: List[String] = List("ci.server", "staging.server", "prod.server")

  def getBandwidth(hostname: String): Future[Int] = Future(hostname.length * 80) // dummy

  /*
    NOT A GOOD SOLUTION, AS WE NEED TO CREATE THE FUTURES ALL THE TIMES, AND WRAP/UNWRAP THEM
    we have
      - a List[String]
      - a function String => Future[Int]
    we want a Future[List[Int]]
   */
  val allBandwidths: Future[List[Int]] = servers.foldLeft(Future(List.empty[Int])) { (accumulator, hostname) =>
    val bandFuture: Future[Int] = getBandwidth(hostname)
    for {
      accBandwidths <- accumulator
      band <- bandFuture
    } yield accBandwidths :+ band
  }

  allBandwidths.foreach(println)

  // BETTER SOLUTION
  val allBandwidthsTraverse: Future[List[Int]] = Future.traverse(servers)(getBandwidth)
  allBandwidthsTraverse.foreach(println)
  // alternative
  // sequence is useful when you want to unwrap a list of Futures into a Future containing a single result
  val allBandwidthsSequence: Future[List[Int]] = Future.sequence(servers.map(getBandwidth))
  allBandwidthsSequence.foreach(println)

  println("====")

  // TODO 1

  import cats.syntax.applicative._
  import cats.syntax.apply._ // mapN

  // general implementation of Traverse for any kind of wrapper type (not just Future), but all for anything that might have Monad in scope
  //  def listTraverse[F[_] : Monad, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
  //    list.foldLeft(List.empty[B].pure[F]) { (wAccumulator, element) => // wrapper accumulator
  //      val wElement: F[B] = func(element) // wrapper element
  //      for {
  //        acc <- wAccumulator
  //        elem <- wElement
  //      } yield acc :+ elem
  //    }
  //
  // instead of Monad, we can also use Applicative, but Applicative does not have access to map and flatMap
  // but we can still combine wAccumulator with wElement with another function: mapN
  def listTraverse[F[_] : Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (wAccumulator, element) => // wrapper accumulator
      val wElement: F[B] = func(element) // wrapper element
      (wAccumulator, wElement).mapN(_ :+ _)
    }
  // as we can use Applicative instead of Monad, it widens up the wrapper types we can use
  // not just Monads, but other things which are not Monad like Validated

  // TODO 2
  def listSequience[F[_] : Applicative, A](list: List[F[A]]): F[List[A]] =
    listTraverse(list)(identity) // instead of x=>x, we can call identity

  // TODO 3

  import cats.instances.vector._

  println(listSequience(List(Vector(1, 2), Vector(3, 4)))) // returns Vector[List[Int]] - all the possible 2-tuples (cartesian product of all possible combination)
  println(listSequience(List(Vector(1, 2), Vector(3, 4), Vector(5, 6)))) // returns Vector[List[Int]] - all the possible 3-pairs (cartesian product of all possible combination)
}
