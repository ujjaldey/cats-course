package part2abstractMath

object _12_UsingMonads extends App {

  import cats.Monad
  import cats.instances.list._

  val monadList = Monad[List] // fetch the implicit Monad[List]
  // Monad has 2 fundamental methods - pure and flatMap
  val aSimpleList = monadList.pure(2) // List(2)
  val anExtendedList = monadList.flatMap(aSimpleList)(x => List(x, x + 1))
  val anExtendedListMap = monadList.map(aSimpleList)(x => x + 1)

  println(aSimpleList)
  println(anExtendedList)
  println(anExtendedListMap)
  // applicable to Option, Try, Future, etc.

  println("====")

  // Either is also a Monad
  // Either is Right biased. So Int would be the type/value for the Either
  val aManualEither: Either[String, Int] = Right(42) // right side is the desirable value, left side is undesirable

  type LoadingOr[T] = Either[String, T]
  type ErrorOr[T] = Either[Throwable, T]
  // the above types are similar to Option[T] or List[T]. Hence these types should be applicable to Monad

  import cats.instances.either._ // so we can also import the instance of Either. it will also cover LoadingOr as this is nothing but Either

  val loadingMonad = Monad[LoadingOr] // uses import cats.instances.either._
  val anEither = loadingMonad.pure(45) // LoadingOr[Int] == Right(45)
  val aChangedLoading = loadingMonad.flatMap(anEither)(n => if (n % 2 == 0) Right(n + 1) else Left("Loading meaning of life..."))
  val aChangedLoadingMap = loadingMonad.map(anEither)(n => if (n % 2 == 0) Right(n + 1) else Left("Loading meaning of life..."))
  println(anEither)
  println(aChangedLoading)
  println(aChangedLoadingMap)

  println("====")

  // imaginary online store
  case class OrderStatus(orderId: Long, status: String)

  def getOrderStatus(orderId: Long): LoadingOr[OrderStatus] = Right(OrderStatus(orderId, "Ready to ship"))

  def trackLocation(orderStatus: OrderStatus): LoadingOr[String] =
    if (orderStatus.orderId > 1000) Left("Not available yet, refreshing data...")
    else Right("Amsterdam, NL")

  val orderId = 457L
  val orderLocation = loadingMonad.flatMap(getOrderStatus(1001L))(orderStatus => trackLocation(orderStatus))
  // or simply:
  //  val orderLocation = loadingMonad.flatMap(getOrderStatus(orderId))(trackLocation)
  println(orderLocation)

  // use extension methods

  import cats.syntax.flatMap._
  import cats.syntax.functor._ // map is here - map is the fundamental to the Functor type class

  val orderLocationBetter: LoadingOr[String] = getOrderStatus(orderId).flatMap(trackLocation) // call flatMap() directly on Monadic value (extension method)
  val orderLocationFor: LoadingOr[String] = for { // or we can use for comprehension
    orderStatus <- getOrderStatus(orderId)
    location <- trackLocation(orderStatus)
  } yield location
  // the extension imports won't matter in this case as Either type belongs to the scala standard library,
  //  and Either already has map and flatMap methods - which are biased towards the Right side (desirable value).

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
        h <- cfg.get("host") // the get method on a Map returns an Option
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
  println(responseOption2) // returns Some(accepted) payload as length < 20

  val responseOptionFor = for { // same implementation using for comprehension
    conn <- OptionHttpService.getConnection(config)
    response <- OptionHttpService.issueRequest(conn, "Hello, HTTP Service")
  } yield response

  println(responseOptionFor)

  println("====")
  println("====")

  // TODO implement another HttpService with LoadingOr or ErrorOr
  object AggresiveHttpService extends HttpService[ErrorOr] { // M[_] is ErrorOr[T] = Either[Throwable, T]
    override def getConnection(cfg: Map[String, String]): ErrorOr[Connection] = // i.e. Either[Throwable, Connection]
      if (!cfg.contains("host") || !cfg.contains("port"))
        Left(new RuntimeException("Connection could not be established: Invalid Configuration")) // Throwable
      else
        Right(Connection(cfg("host"), cfg("port"))) // no chance of blowing up, as we have already checked .contains() above

    override def issueRequest(connection: Connection, payload: String): ErrorOr[String] = // i.e. Either[Throwable, String]
      if (payload.length >= 20) Left(new RuntimeException("Payload is too large")) // Throwable
      else Right(s"Request ($payload) has been accepted")
  }

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

  println("====")
  println("====")

  // Generalize

  import cats.instances.option._

  // here we can use the for comprehension on any kind of M data structure (Option, ErrorOr, etc.) as long as you have
  //  an implicit Monad in scope. So, you are able to use the for comprehension regardless of what type your
  //  HttpService deals with.
  def getResponse[M[_]](service: HttpService[M], payload: String)(implicit monad: Monad[M]): M[String] = {
    // OR. use shorthand for the implicit Monad: or def getResponse[M[_] : Monad](service: HttpService[M], payload: String): M[String] = {
    for { // as you have extension methods in scope, you would be able to directly use for comprehension directly in this method
      conn <- service.getConnection(config)
      response <- service.issueRequest(conn, payload)
    } yield response
  }

  // exact same high level api can be used, regardless of what implementation of HttpService and what type of HttpService we are using
  println(getResponse(OptionHttpService, "Hello ErrorOrServer LONG LONG")) // as we have import cats.instances.option._
  println(getResponse(OptionHttpService, "Hello httpService"))
  println(getResponse(AggresiveHttpService, "Hello ErrorOrServer LONG LONG")) // as we have import cats.instances.either._ which covers EitherOr
  println(getResponse(AggresiveHttpService, "Hello OptionHttpSer"))
  println("====")
}
