package part3datamanipulation

object _15_Readers extends App {
  /*
    - configuration file => initial data structure
    - a db layer
    - an http layer
    - a business logic layer

    The configuration file feeds all the layers in your application
   */
  case class Configuration(dbUsername: String, dbPassword: String, host: String, port: Int, nThreads: Int, emailReplyTo: String)

  case class DbConnection(username: String, password: String) {
    // implementations are really that important
    def getOrderStatus(orderId: Long): String = "dispatched" // select * from db table and return the status of the orderId

    def getLastOrderId(username: String): Long = 123456 // select max(orderId) from table where username = username
  }

  case class HttpService(host: String, port: Int) {
    def start(): Unit = println("server started") // this would start the actual server
  }

  // bootstrap
  val config = Configuration("ujjal", "udey123", "localhost", 1234, 8, "ud.sender@gmail.com")


  import cats.data.Reader
  // Reader is a data processing type from Cats that implements informing the creation of a DbConnection and HttpService based on the Configuration object

  // in Reader, the first type is an input, the second type is the output (here, Configuration is input, DbConnection is output)
  // it gets the Configuration, and then returns a DbConnection
  // Reader.apply() factory method takes a function from a configuration to a DbConnection
  val dbReader: Reader[Configuration, DbConnection] = Reader(conf => DbConnection(conf.dbUsername, conf.dbPassword))
  val dbConn = dbReader.run(config) // pass the Configuration object in the run() which will run the function
  // and return a DbConnection object

  // Reader[I, O] - but if we use a map() method, then O can be transformed into something else
  // dbReader returns a DbConnection. So we can use map, and then use getOrderStatus() method to return the order status
  val ujjalsOrderStatusReader: Reader[Configuration, String] = dbReader.map(dbConn => dbConn.getOrderStatus(55))
  // then run the Reader to get the actual status
  val ujjalsOrderStatus: String = ujjalsOrderStatusReader.run(config)
  println(ujjalsOrderStatus)
  // we pass the config which was created all the way up at the bootstrap stage and we obtain something
  // very particular in the middle of the application

  def getLastOrderStatus2(username: String): String = {
    val usersLastOrderIdReader: Reader[Configuration, String] = dbReader // the 2nd parameter type is the type returned by getOrderStatus() function
      .map(_.getLastOrderId(username)) // conn => conn.getLastOrderId(username)
      .flatMap(lastOrderId => dbReader.map(_.getOrderStatus(lastOrderId))) // flatMap takes a function from the type of value the first reader obtained and runs another reader on top of that
    // instead of calling the run() method of usersLastOrderIdReader and getting the lastOrderId,
    // and then calling getOrderStatus() for that order id, we can use flatMap.
    // Reader also supports map and flatMap

    usersLastOrderIdReader.run(config) // input is Configuration
  }

  // identical. using for comprehension
  // as Reader supports map and flatMap, it supports for comprehension
  def getLastOrderStatus(username: String): String = {
    val usersOrderFor: Reader[Configuration, String] = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
    } yield orderStatus

    usersOrderFor.run(config)
  }

  println("1 =>", getLastOrderStatus2("ujjal"))
  println("2 =>", getLastOrderStatus("ujjal"))

  /*
    Pattern
    1. you create the initial data structure
    2. you create a reader which specifies how that data structure be manipulated later through functions
    3. you can then map & flatMap the reader to produce derived Information
    4. when you need the final piece of information, you call run on the reader with the initial data structure
   */

  // TODO 1 - email a user
  case class EmailService(emailReplyTo: String) {
    def sendEmail(address: String, contents: String) = s"From: $emailReplyTo; to: $address >>> $contents"
  }

  def emailUser(username: String, userEmail: String): String = {
    // fetch the status of their last order
    // email them with the Email service: "Your last order has the status: (status)"

    // we have a Reader, that given a Configuration will create a EmailService
    val emailServiceReader: Reader[Configuration, EmailService] = Reader(conf => EmailService(conf.emailReplyTo))

    val emailReader: Reader[Configuration, String] = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
      emailService <- emailServiceReader
    } yield emailService.sendEmail(userEmail, s"Your last order ($lastOrderId) has the status: $orderStatus")

    emailReader.run(config)
  }

  println(emailUser("ujjal", "ud@gmail.com"))

  // TODO 2 - what programming pattern do Readers remind you of?
  // Dependency Injection!

  // create some initial data structure, in the middle of the application you decide how the information is going to flow
  // but you have not supplied with the initial data structure
  // and at the very end, you can run and inject the initial data structure

  // this is how you can implement dependency injection in a purely functional way using Cats
}
