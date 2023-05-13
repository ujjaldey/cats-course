package part2abstractMath

object UsingMonads extends App {

  import cats.Monad
  import cats.instances.list._

  val monadList = Monad[List] // fetch the implicit Monad[List]
  val aSimpleList = monadList.pure(2) // List(2)
  val anExtendedList = monadList.flatMap(aSimpleList)(x => List(x, x + 1))

  println(aSimpleList)
  println(anExtendedList)
  // applicable to Option, Try, Future, etc.

  // Either is also a Monad
  // Either is Right biased. So Int would be the type/value for the Either
  val aManualEither: Either[String, Int] = Right(42) // right side is the desirable value, left side is undesirable

  type LoadingOr[T] = Either[String, T]
  type ErrorOr[T] = Either[Throwable, T]
  // the above types are similar to Option[T] or List[T]. Hence types should be applicable to Monad

  import cats.instances.either._

  val loadingMonad = Monad[LoadingOr] // uses import cats.instances.either._
  val anEither = loadingMonad.pure(45) // LoadingOr[Int] == Right(45)
  val aChangedLoading = loadingMonad.flatMap(anEither)(n => if (n % 2 == 0) Right(n + 1) else Left("Loading meaning of life..."))
  println(anEither)
  println(aChangedLoading)

  println("====")

  // imaginary online store
  case class OrderStatus(orderId: Long, status: String)

  def getOrderStatus(orderId: Long): LoadingOr[OrderStatus] = Right(OrderStatus(orderId, "Ready to ship"))

  def trackLocation(orderStatus: OrderStatus): LoadingOr[String] =
    if (orderStatus.orderId > 1000) Left("Not available yet, refreshing data...")
    else Right("Amsterdam, NL")

  val orderId = 457L
  val orderLocation = loadingMonad.flatMap(getOrderStatus(orderId))(orderStatus => trackLocation(orderStatus))
  // or simply:
  //  val orderLocation = loadingMonad.flatMap(getOrderStatus(orderId))(trackLocation)

  // use extension methods

  import cats.syntax.flatMap._
  import cats.syntax.functor._ // map is here

  val orderLocationBetter: LoadingOr[String] = getOrderStatus(orderId).flatMap(trackLocation) // call flatMap() directly on Monadic value (extension method)
  val orderLocationFor: LoadingOr[String] = for {
    orderStatus <- getOrderStatus(orderId)
    location <- trackLocation(orderStatus)
  } yield location

  println(orderLocationBetter)
  println(orderLocationFor)
  println("====")

  // TODO: the service layer API of a web app
  case class Connection(host: String, port: String)

  val config = Map("host" -> "localhost", "port" -> "4840")

  trait HttpService[M[_]] {
    def getConnection(cfg: Map[String, String]): M[Connection]

    def issueRequest(connection: Connection, payload: String): M[String]
  }

  // DO NOT CHANGE THE CODE

  /*
      Requirements:
      - if the host and port are found in the configuration map, then we'll return a M containing a connection with those values
        otherwise the method will fail, according to the logic of the type M
        (for Try it will return a Failure, for Option it will return None, for Future it will be a failed Future, for Either it will return a Left)
      - the issueRequest method returns a M containing the string: "request (payload) has been accepted", if the payload is less than 20 characters
        otherwise the method will fail, according to the logic of the type M

      TODO: provide a real implementation of HttpService using Try, Option, Future, Either
     */
  object OptionHttpService extends HttpService[Option] {
    override def getConnection(cfg: Map[String, String]): Option[Connection] =
      for {
        h <- cfg.get("host")
        p <- cfg.get("port")
      } yield Connection(h, p)

    override def issueRequest(connection: Connection, payload: String): Option[String] =
      if (payload.length >= 20) None
      else Some(s"Request ($payload) has been accepted")
  }

  val responseOption = OptionHttpService.getConnection(config).flatMap {
    conn => OptionHttpService.issueRequest(conn, "Hello from OptionHttpService")
  }
  println(responseOption) // returns None as length >= 20

  val responseOption2 = OptionHttpService.getConnection(config).flatMap {
    conn => OptionHttpService.issueRequest(conn, "Hello HttpService")
  }
  println(responseOption2)

  val responseOptionFor = for {
    conn <- OptionHttpService.getConnection(config)
    response <- OptionHttpService.issueRequest(conn, "Hello, HTTP Service")
  } yield response

  println(responseOptionFor)

  // TODO implement another HttpService with LoadingOr or ErrorOr
  object AggresiveHttpService extends HttpService[ErrorOr] {
    override def getConnection(cfg: Map[String, String]): ErrorOr[Connection] =
      if (!cfg.contains("host") || !cfg.contains("port"))
        Left(new RuntimeException("Connection could not be established: Invalid Configuration"))
      else
        Right(Connection(cfg("host"), cfg("port"))) // no chance of blowing up, as we have already checked .contains() above

    override def issueRequest(connection: Connection, payload: String): ErrorOr[String] =
      if (payload.length >= 20) Left(new RuntimeException("Payload is too large"))
      else Right(s"Request ($payload) has been accepted")
  }

  println("====")

  val errorOrResponse1: ErrorOr[String] = for {
    conn <- AggresiveHttpService.getConnection(config)
    response <- AggresiveHttpService.issueRequest(conn, "Hello ErrorOrServer")
  } yield response

  println(errorOrResponse1)

  val errorOrResponse2: ErrorOr[String] = for {
    conn <- AggresiveHttpService.getConnection(config)
    response <- AggresiveHttpService.issueRequest(conn, "Hello ErrorOrServer LONG LONG")
  } yield response

  println(errorOrResponse2)

  // Generalize

  import cats.instances.option._

  def getResponse[M[_]](service: HttpService[M], payload: String)(implicit monad: Monad[M]): M[String] = {
    // as you have extension methods in scope, you would be able to directly use for comprehension directly in this method
    for {
      conn <- service.getConnection(config)
      response <- service.issueRequest(conn, payload)
    } yield response
  }

  println("====")
  // exact same high level api can be used, regardless of what implementation of HttpService and what type of HttpService we are using
  println(getResponse(OptionHttpService, "Hello ErrorOrServer LONG LONG"))
  println(getResponse(OptionHttpService, "Hello httpService"))
  println(getResponse(AggresiveHttpService, "Hello ErrorOrServer LONG LONG"))
  println(getResponse(AggresiveHttpService, "Hello OptionHttpSer"))
  println("====")
}
