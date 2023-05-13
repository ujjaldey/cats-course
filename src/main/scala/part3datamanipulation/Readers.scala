package part3datamanipulation

object Readers extends App {
  /*
    - configuration file => initial data structure
    - a db layer
    - an http layer
    - a business logic layer
   */
  case class Configuration(dbUsername: String, dbPassword: String, host: String, port: Int, nThreads: Int, emailReplyTo: String)

  case class DbConnection(username: String, password: String) {
    def getOrderStatus(orderId: Long): String = "dispatched" // select * from db table and return the status of the orderId

    def getLastOrderId(username: String): Long = 123456 // select max(orderId) from table where username = username
  }

  case class HttpService(host: String, port: Int) {
    def start(): Unit = println("server started") // this would start the actual server
  }

  // bootstrap
  val config = Configuration("ujjal", "udey123", "localhost", 1234, 8, "ud.sender@gmail.com")
  // cats Reader

  import cats.data.Reader

  val dbReader: Reader[Configuration, DbConnection] = Reader(conf => DbConnection(conf.dbUsername, conf.dbPassword)) // Configuration is input, DbConnection is output
  val dbConn = dbReader.run(config)

  // Reader[I, O] - but if we use a map() method, then O can be transformed into something else
  val ujjalsOrderStatusReader: Reader[Configuration, String] = dbReader.map(dbConn => dbConn.getOrderStatus(55))
  val ujjalsOrderStatus: String = ujjalsOrderStatusReader.run(config)
  println(ujjalsOrderStatus)

  def getLastOrderStatus(username: String): String = {
    //    val usersLastOrderIdReader: Reader[Configuration, String] = dbReader
    //      .map(_.getLastOrderId(username))
    //      .flatMap(lastOrderId => dbReader.map(_.getOrderStatus(lastOrderId)))
    //    usersLastOrderIdReader.run(config)

    // identical. using for comprehension
    val usersOrderFor: Reader[Configuration, String] = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
    } yield orderStatus

    usersOrderFor.run(config)
  }

  println(getLastOrderStatus("ujjal"))

  /*
    Pattern
    1. you create the initial data structure
    2. you create a reader which specifies how that data structure be manipulated later
    3. you can then nap & flatMap the reader to produce derived Information
    4. when you need the final piece of information, you catt run on the reader with the initial data structure
   */

  // TODO 1 - email a user
  case class EmailService(emailReplyTo: String) {
    def sendEmail(address: String, contents: String) = s"From: $emailReplyTo; to: $address >>> $contents"
  }

  def emailUser(username: String, userEmail: String): String = {
    // fetch the status of their last order
    // email them with the Email service: "Your last order has the status: (status)"
    val emailServiceReader: Reader[Configuration, EmailService] = Reader(conf => EmailService(conf.emailReplyTo))
    val emailReader: Reader[Configuration, String] = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
      emailService <- emailServiceReader
    } yield emailService.sendEmail(userEmail, s"Your last order has the status: $orderStatus")

    emailReader.run(config)
  }

  println(emailUser("ujjal", "ud@gmail.com"))

  // TODO 2 - what programming pattern do Readers remind you of?
  // Dependency Injection!
}
