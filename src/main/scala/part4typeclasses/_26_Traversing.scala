package part4typeclasses

import cats.{Applicative, Foldable, Functor, Monad}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}


// Traversable provide a higher level approach to iterations
object _26_Traversing extends App {
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val servers: List[String] = List("ci.server", "staging.server", "prod.server")

  def getBandwidth(hostname: String): Future[Int] = Future(hostname.length * 80) // dummy

  // get all the bandwidths
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
      accBandwidths <- accumulator // List[Int]
      band <- bandFuture
    } yield accBandwidths :+ band // append a Int to a List[Int]
  }

  allBandwidths.foreach(println)

  // BETTER SOLUTION
  // traverse takes some container and a function that turns every element into Future of something else
  // and it will return something that we wanted
  // here the traverse will automatically return a Future[List[Int]] by combining the intermediate
  // values to a single list.
  // traverse also uses foldLeft internally
  val allBandwidthsTraverse: Future[List[Int]] = Future.traverse(servers)(getBandwidth)
  allBandwidthsTraverse.foreach(println)

  // alternative
  // sequence takes a container of Futures and returns a Future of containers.
  // servers.map(getBandwidth) returns a List[Future] and if you do sequence on that, it will
  // return a Future[List].
  // sequence is useful when you want to unwrap a List of Futures into a Future of a List
  val allBandwidthsSequence: Future[List[Int]] = Future.sequence(servers.map(getBandwidth))
  allBandwidthsSequence.foreach(println)

  println("======")

  // TODO 1

  // pure

  // applicative

  import cats.syntax.applicative._
  // flatMap
  import cats.syntax.flatMap._
  // map
  import cats.syntax.functor._
  // mapN
  import cats.syntax.apply._

  // general implementation of Traverse for any kind of wrapper type (not just Future), but all for anything that might have Monad in scope
  //  def listTraverse[F[_] : Monad, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
  //    list.foldLeft(List.empty[B].pure[F]) { (wAccumulator, element) => // wrapper accumulator
  //      val wElement: F[B] = func(element) // wrapper element
  //      for {
  //        acc <- wAccumulator
  //        elem <- wElement
  //      } yield acc :+ elem
  //    }

  // this operates on any higher ordered type provided we have a Monad in scope.
  // F[_] : Monad means there is an implicit Monad[F] as a parameter
  def listTraverseX[F[_] : Monad, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
  // List.empty[B] wrapped in F, so we call te pure method
    list.foldLeft(List.empty[B].pure[F]) { (wAccumulator, element) => // wrapper accumulator
      val wElement: F[B] = func(element) // wrapper element
      for {
        acc <- wAccumulator
        elem <- wElement
      } yield acc :+ elem // B appended to List[B]
    }

  // instead of Monad, we can also use Applicative, but Applicative does not have access to map and flatMap
  // but we can still combine wAccumulator with wElement with another function: mapN

  // listTraverseX has a lower minimum bound, not as a Monad, but as weaker monad i.e. Applicative
  // being applicative, it does not have access to map and flatMap, but mapN can be used
  // (part of weaker applicative)
  // mapN is applied to a Tuple of wrappers and applies a function on the values inside.
  // and the result is another wrapper with the result of that function.
  def listTraverse[F[_] : Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (wAccumulator, element) => // wrapper accumulator
      val wElement: F[B] = func(element) // wrapper element
      (wAccumulator, wElement).mapN(_ :+ _) // B appended to List[B]
    }

  // this proves that the minimum requirement for this list traverse method to work is the
  // presence of an Applicative, not a Monad. it widens up the wrapper types we can use
  // not just Monads, but other things which are not Monad like Validated

  // TODO 2
  // sequence is same as traverse, without the function
  // F has an implicit Applicative in scope.
  def listSequence[F[_] : Applicative, A](list: List[F[A]]): F[List[A]] =
  // in listTraverse we need to pass a function. but here, it should be a function for an element
  // to be converted to itself. i.e. x=>x
  //    listTraverse(list)(x => x)
    listTraverse(list)(identity) // instead of x=>x, we can call identity

  // TODO 3 - what's the result of

  import cats.instances.vector._

  println(listSequence(List(Vector(1, 2), Vector(3, 4)))) // requires Applicative[Vector] in scope
  // returns Vector[List[Int]] - all the possible 2-tuples (cartesian product of all possible combination)
  println(listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6))))
  // returns Vector[List[Int]] - all the possible 3-pairs (cartesian product of all possible combination)

  println("=======")

  // test that the predicate satisfies for all the elements in the list. If some element do not satisfy,
  // then this will return a None.
  // this is equivalent to forall() method

  import cats.instances.option._

  def filterAsOptionX(list: List[Int])(predicate: Int => Boolean): Option[List[Int]] =
    listTraverse[Option, Int, Int](list)(n => if (predicate(n)) Some(n) else None)
  // but it's better to replace this with filter

  def filterAsOption(list: List[Int])(predicate: Int => Boolean): Option[List[Int]] =
    listTraverse[Option, Int, Int](list)(n => Some(n).filter(predicate)) // requires cats.instances.option._

  // TODO 4 - what's the result of
  println(filterAsOption(List(2, 4, 6))(_ % 2 == 0))
  // Some(List(2,4,6) - the predicate returns true for all elements.
  // when filterAsOption implemented in terms of traverse which is implemented in terms of foldLeft,
  //  every new element is being combined with the accumulator as a wrap.
  // so we are combining 3 Some() instances to a Some[List]

  println(filterAsOption(List(1, 2, 3))(_ % 2 == 0))
  // None - for 1 and 3, the predicate returns false, hence the Option becomes None.
  // here for 1 and 3, the predicate returns false. As listTraverse is implemented in terms of
  // foldLeft and mapN combination function, we are combining some instances with None.
  // when you combine a Some with a None, you get None. So the output will be None.

  // this is equivalent to forall method of the List
  println(List(2, 4, 6).forall(_ % 2 == 0))
  println(List(1, 2, 3).forall(_ % 2 == 0))
  println("==========")

  // listTraverse is useful in presence of some Applicatives that are not Monads e.g. Validated

  // there is an instance of Applicative for Validated, but no Monad.

  import cats.data.Validated

  // imports Semigroup[List], so we can generate Applicative[ErrorsOr]
  import cats.instances.list._

  type ErrorsOr[T] = Validated[List[String], T]

  def filterAsValidated(list: List[Int])(predicate: Int => Boolean): ErrorsOr[List[Int]] = // Validated[List[String], List[Int]]
    listTraverse[ErrorsOr, Int, Int](list) { n =>
      if (predicate(n)) Validated.valid(n)
      else Validated.invalid(List(s"predicate for $n failed"))
    } // requires cats.instances.list._

  // TODO 5 - what's the result of
  println(filterAsValidated(List(2, 4, 6))(_ % 2 == 0))
  // all the 3 elements passes in the predicate. So all the returns Valid.
  // hence the combined value is going to be: Valid(List(2,4,6))

  println(filterAsValidated(List(1, 2, 3))(_ % 2 == 0))
  // Invalid(List(predicate for 1 failed, predicate for 3 failed))
  // for every value the predicate returns false, would generate an independent list of Invalid (error).
  // and these Validated instances will be combined as according to mapN function and
  // return a Invalid with combined set of errors.

  // in this case, Validated is a useful instance of a data type for which no Monad in scope, but just Applicative.
  // so it's worth making listTraverse as an Applicative (not a stronger restriction for Monad)

  // when designing an API, make sure to create the methods such that it requires
  // least restriction, not most convenient one.


  // we can generalize the listTraverse to any type of container, not just List:
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

    import cats.Id // same as Identity: type Id[A] = A. Cats will create an implicit val of catsInstancesForId
    // which contains a BiMonad which is a very strong type of Monad.
    // when wè use cats.Id, cats will automatically create an implicit Applicative that can be injected in the scope.


    // MyTraverse can naturally implement the map method which is specific to a Functor.
    // so we can safely mixin the Functor trait along with MyTraverse above.
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
  val allBandwidthCats = Traverse[List].traverse(servers)(getBandwidth)
  // requires implicit Applicative[Future] from import cats.instances.future._
  allBandwidthCats.foreach(println)

  println("====")

  // extension methods

  import cats.syntax.traverse._ // sequence + traverse

  val allBandwidthCats2 = servers.traverse(getBandwidth)
  allBandwidthCats2.foreach(println)
}
