package part2abstractMath


import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object MonadTransformers extends App {
  def sumAllOptions(values: List[Option[Int]]): Int = ???

  // option transformer

  import cats.data.OptionT
  import cats.instances.future._
  // fetch an implicit OptionT[List]
  import cats.instances.list._

  val listOfNumberOptions: OptionT[List, Int] = OptionT(List(Option(1), Option(2)))
  val listOfCharOptions: OptionT[List, Char] = OptionT(List(Option('a'), Option('b'), Option.empty[Char]))

  val listOfTuples: OptionT[List, (Int, Char)] = for {
    char <- listOfCharOptions
    number <- listOfNumberOptions
  } yield (number, char)

  println(listOfTuples.value)

  // either transformer

  import cats.data.EitherT

  val listOfEithers: EitherT[List, String, Int] = EitherT(List(Left("something wrong"), Right(43), Right(2)))
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val futureOfEither: EitherT[Future, String, Int] = EitherT.right(Future(45)) // wrap over Future(Right(45))
  // or: EitherT[Future[Either[String, Int]]](Right(45))

  /*
    TODO exercise
    We have a multi-machine cluster for your business which will receive a traffic surge following a media appearance.
    We measure bandwidth in units.
    We want to allocate TWO of our servers to cope with the traffic spike.
    We know the current capacity for each server and we know we'll hold the traffic if the sum of bandwidths is > 250.
   */
  val bandwidths = Map(
    "server1.abcd.com" -> 50,
    "server2.abcd.com" -> 300,
    "server3.abcd.com" -> 170
  )

  // wrapper over Future[Either[String, T]]
  type AsyncResponse[T] = EitherT[Future, String, T] // of type Future, desired value of Either is T, undesired value is String

  def getBandwidth(server: String): AsyncResponse[Int] = bandwidths.get(server) match {
    case None => EitherT.left(Future(s"Server $server unreachable"))
    case Some(b) => EitherT.right(Future(b))
  }

  // TODO 1
  // hint: call getBandwidth twice, ad combine the results
  // to import F: Monad[Future]

  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] = for {
    band1 <- getBandwidth(s1)
    band2 <- getBandwidth(s2)
  } yield band1 + band2 > 250
  // returns Future[Either[String, Boolean]]

  // TODO 2
  // hint: call canWithstandSurge + transform
  def generateTrafficSpikeReport(s1: String, s2: String): AsyncResponse[String] =
    canWithstandSurge(s1, s2).transform {
      case Left(reason) => Left(s"Servers $s1 and $s2 cannot cope with the incoming spike: $reason")
      case Right(false) => Left(s"Servers $s1 and $s2 cannot cope with the incoming spike: not enough total bandwidth")
      case Right(true) => Right(s"Servers $s1 and $s2 can cope with the incoming spike: no problem")
    }
  // returns Future[Either[String, String]]

  generateTrafficSpikeReport("server1.abcd.com", "server3.abcd.com").value.foreach(println)
  generateTrafficSpikeReport("server5.abcd.com", "server3.abcd.com").value.foreach(println)
}
