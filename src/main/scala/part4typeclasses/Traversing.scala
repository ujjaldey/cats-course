package part4typeclasses

import cats.{Applicative, Foldable, Functor, Monad}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}


// Traversable provide a higher level approach to iterations
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
  // sequence is useful when you want to unwrap a list of Futures into a Future of a list
  val allBandwidthsSequence: Future[List[Int]] = Future.sequence(servers.map(getBandwidth))
  allBandwidthsSequence.foreach(println)

  println("====")

  // TODO 1

  // pure

  import cats.syntax.applicative._
  // flatMap
  import cats.syntax.flatMap._
  // map
  import cats.syntax.apply._
  import cats.syntax.functor._ // mapN

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

  def listTraverseX[F[_] : Monad, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (wAccumulator, element) => // wrapper accumulator
      val wElement: F[B] = func(element) // wrapper element
      for {
        acc <- wAccumulator
        elem <- wElement
      } yield acc :+ elem
    }

  // listTraverseX has a lower bound not as a Monad, but as weaker monad i.e. applicative
  // being applicative, it does not have access to map and flatMap, but mapN can be used (part of weaker applicative)
  def listTraverse[F[_] : Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (wAccumulator, element) => // wrapper accumulator
      val wElement: F[B] = func(element) // wrapper element
      (wAccumulator, wElement).mapN(_ :+ _)
    }
  // as we can use Applicative instead of Monad, it widens up the wrapper types we can use
  // not just Monads, but other things which are not Monad like Validated

  // TODO 2
  def listSequence[F[_] : Applicative, A](list: List[F[A]]): F[List[A]] =
    listTraverse(list)(identity) // instead of x=>x, we can call identity

  // TODO 3 - what's the result of

  import cats.instances.vector._

  println(listSequence(List(Vector(1, 2), Vector(3, 4)))) // returns Vector[List[Int]] - all the possible 2-tuples (cartesian product of all possible combination)
  println(listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6)))) // returns Vector[List[Int]] - all the possible 3-pairs (cartesian product of all possible combination)

  println("====")

  // test that the predicate satisfies for all the elements in the list. If some element do not satisfy, then this will return a None
  // this is equivalent to forall() method
  def filterAsOption(list: List[Int])(predicate: Int => Boolean): Option[List[Int]] =
    listTraverse[Option, Int, Int](list)(n => Some(n).filter(predicate)) // requires cats.instances.option._

  // TODO 4 - what's the result of
  println(filterAsOption(List(2, 4, 6))(_ % 2 == 0)) // Some(List(2,4,6) - the predicate returns true for all elements. So the foldLeft works and return the Option of the consolidated list
  println(filterAsOption(List(1, 2, 3))(_ % 2 == 0)) // None - for 1 and 3, the predicate returns false, hence the Option becomes None. So foldLeft will become None too
  println(List(1, 2, 3).forall(_ % 2 == 0))
  println(List(2, 4, 6).forall(_ % 2 == 0))
  println("====")

  // listTraverse is useful in presence of some Applicatives that are not Monads e.g. Validated

  import cats.data.Validated
  import cats.instances.list._

  type ErrorsOr[T] = Validated[List[String], T]

  def filterAsValidated(list: List[Int])(predicate: Int => Boolean): ErrorsOr[List[Int]] =
    listTraverse[ErrorsOr, Int, Int](list) { n =>
      if (predicate(n)) Validated.valid(n)
      else Validated.invalid(List(s"predicate for $n failed"))
    } // requires cats.instances.list._

  // TODO 5 - what's the result of
  println(filterAsValidated(List(2, 4, 6))(_ % 2 == 0)) // Valid(List(2,4,6))
  println(filterAsValidated(List(1, 2, 3))(_ % 2 == 0)) // Invalid(List(predicate for 1 failed, predicate for 3 failed))

  // use L instead of List
  // MyTraverse can naturally implement the map method which is specific to a Functor. So it is safe to mix in Functor
  trait MyTraverse[L[_]] extends Foldable[L] with Functor[L] {
    // same as listTraverse above
    // traverse method is not implemented, as it depends on the empty container.
    // the actual implementation should take care of defining the traverse method
    def traverse[F[_] : Applicative, A, B](container: L[A])(func: A => F[B]): F[L[B]]

    // same as listSequence above
    def sequence[F[_] : Applicative, A](container: L[F[A]]): F[L[A]] =
      traverse(container)(identity)

    // TODO 6
    // hint
    //type Identity[T] = T // cats identifies there is an Applicative for the fake type Identity
    // there is such a thing called cats.Id

    import cats.Id

    def map[A, B](wa: L[A])(f: A => B): L[B] = {
      // function f takes A and returns B (not container of B)
      // but we are tricking the compiler by using Identifier
      //      traverse[Identity, A, B](wa)(f)
      traverse[Id, A, B](wa)(f)
    }
  }

  // should be same implementation as MyTraverse above

  import cats.Traverse
  import cats.instances.future._ // Applicative[Future]

  // actual problem with the server bandwidth
  val allBandwidthCats = Traverse[List].traverse(servers)(getBandwidth) // requires import cats.instances.future._
  allBandwidthCats.foreach(println)

  println("====")

  // extension methods

  import cats.syntax.traverse._ // sequence + traverse

  val allBandwidthCats2 = servers.traverse(getBandwidth)
  allBandwidthCats2.foreach(println)
}
