package part3datamanipulation

object Readers extends App {

  /*
    Let's say we have
    - configuration file => initial data structure
    - a DB layer
    - an HTTP layer
    - a business logic layer
   */

  case class Configuration(
      dbUsername: String,
      dbPassword: String,
      host: String,
      port: Int,
      nThreads: Int
  )
  case class DbConnection(username: String, password: String) {
    def getOrderStatus(orderId: Long): String =
      "dispatched" // select * from the db table and returnn the status of the orderId
    def getLastOrderId(username: String): Long = 541234
  }
  case class HttpService(host: String, port: Int) {
    def start(): Unit = println("server started")
  }

  // bootstrap
  val config = Configuration("daniel", "rockthejvm1!", "localhost", 1234, 8)
  // cats reader
  // we can define a reader that defines an input (in this case Configuration) and an ouptut (DbConnection). This will declare what's needed
  // in our application for creating a DbConnection
  import cats.data.Reader
  val dbReader: Reader[Configuration, DbConnection] =
    Reader(config => DbConnection(config.dbUsername, config.dbPassword))
  val dbConn = dbReader.run(config)

  // Good thing about Readers is that you can map them
  val danielsOrderStatusReader: Reader[Configuration, String] =
    dbReader.map(_.getOrderStatus(55))
  val danielOrderStatus = danielsOrderStatusReader.run(config)

  def getLastOrderStatus(username: String): String = {
    val usersOrderFor = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
    } yield orderStatus

    usersOrderFor.run(config)
  }
  
  /*
    Pattern:
      1. you create the initial data structure
      2. you create a reader which specifies how that data structure will be manipulated later
      3. you can then map & flatMap the reader to produce derived information
      4. when you need the final piece of information, you call run on the reader with the initial data structure
   */
}
