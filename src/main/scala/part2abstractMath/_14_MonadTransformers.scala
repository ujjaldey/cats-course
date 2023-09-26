package part2abstractMath


import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object _14_MonadTransformers extends App {
  def sumAllOptions(values: List[Option[Int]]): Int = ???
  // here to calculate, we need to unwrap every single options in the list and then wrap it back

  // in such situations, Monad transformers will allow us to use these nested/combined monads, and
  //  then just apply map or flatMap on combination of these without the need of unwrapping the monads all the time

  // option transformer

  // OptionT[F[_], A] is a light wrapper on an F[Option[A]]

  import cats.data.OptionT
  import cats.instances.future._
  import cats.instances.list._ // fetch an implicit OptionT[List]

  // OptionT[List, Int] means a List of Option of Int
  val listOfNumberOptions: OptionT[List, Int] = OptionT(List(Option(1), Option(2)))
  val listOfCharOptions: OptionT[List, Char] = OptionT(List(Option('a'), Option('b'), Option.empty[Char]))

  val listOfTuples: OptionT[List, (Int, Char)] = for { // Type OptionT has map() and flatMap(), so we can use for comprehension
    char <- listOfCharOptions
    number <- listOfNumberOptions
  } yield (number, char)
  // this is a convenience API, you don't have to unwrap your inner options all the time
  // Monad Transformer will provide you with map and flatMap so that when you have a wrapped monad over your own monad,
  //  you dont need to unwrap your inner monads all the time

  println(listOfTuples.value) // value accesses the List[Option] inside

  // either transformer

  import cats.data.EitherT

  val listOfEithers: EitherT[List, String, Int] = EitherT(List(Left("something wrong"), Right(43), Right(2)))
  // EitherT is similar to OptionT. use it for List[Either[A,B]] (rather F[Either[A,B]])

  println(listOfEithers.value)
  println((for {
    x <- listOfEithers
  } yield s"^$x^").value) // will be applied only on Right

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  // EitherT.right(Future(45)) is the convenient method (EitherT.right or EitherT.left) over: EitherT(Future[Either[String, Int]](Right(45)))
  // it will also need import cats.instances.future._
  val futureOfEither: EitherT[Future, String, Int] = EitherT.right(Future(45)) // wrap over Future(Right(45))
  // or: EitherT[Future[Either[String, Int]]](Right(45))
  // or: Either of Either

  /*
    TODO exercise
    We have a multi-machine cluster for your business which will receive a traffic surge following a
      media appearance.
    We measure bandwidth in units.
    We want to allocate TWO of our servers to cope with the traffic spike.
    We know the current capacity for each server and we know we'll hold the traffic if the sum of
      bandwidths is > 250.
   */
  val bandwidths = Map(
    "server1.abcd.com" -> 50,
    "server2.abcd.com" -> 300,
    "server3.abcd.com" -> 170
  )

  // wrapper over Future[Either[String, T]]
  type AsyncResponse[T] = EitherT[Future, String, T]
  // Future[Either[String, T]] -- of type Future, desired value of Either is T, undesired value is String

  def getBandwidth(server: String): AsyncResponse[Int] = bandwidths.get(server) match {
    case None => EitherT.left(Future(s"Server $server unreachable")) // or EitherT(Future[String, Int])(Left(Future(s"Server $server unreachable")))
    case Some(b) => EitherT.right(Future(b))
  }

  // TODO 1
  // hint: call getBandwidth twice, and combine the results
  // to import F: Monad[Future]

  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] = for {
    band1 <- getBandwidth(s1) // needs import cats.instances.future._
    band2 <- getBandwidth(s2) // needs import cats.instances.future._
  } yield band1 + band2 > 250
  // returns Future[Either[String, Boolean]]

  // TODO 2
  // hint: call canWithstandSurge + transform
  def generateTrafficSpikeReport(s1: String, s2: String): AsyncResponse[String] =
  // if canWithstandSurge returns a failure (Left), display the reason (e.g. unreachable)
  // if canWithstandSurge returns success but false, that means not enough bandwidth
  // if canWithstandSurge returns success and true, that means the servers can handle the spike
  // the transform() method takes a function which transform the Either[A,B] to Either[C,D] (f: Either[A, B] => Either[C, D])
    canWithstandSurge(s1, s2).transform {
      case Left(reason) => Left(s"1> Servers $s1 and $s2 cannot cope with the incoming spike: $reason") // unreachable server
      case Right(false) => Left(s"2> Servers $s1 and $s2 cannot cope with the incoming spike: not enough total bandwidth") // desirable result is false, as not enough bandwidth
      case Right(true) => Right(s"3> Servers $s1 and $s2 can cope with the incoming spike: no problem") // desirable result is true, with sufficient bandwidth
    }
  //  ^^^^^^^^^^^^^^^^^^^ =>                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  // Future[Either[String, Boolean]]  transform) =>  Future[Either[String, String]]

  println("====")

  generateTrafficSpikeReport("server2.abcd.com", "server3.abcd.com").value.foreach(println)
  generateTrafficSpikeReport("server1.abcd.com", "server3.abcd.com").value.foreach(println)
  generateTrafficSpikeReport("server5.abcd.com", "server3.abcd.com").value.foreach(println)
}
