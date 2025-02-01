package part2abstractMath

object UsingMonads extends App {

  import cats.Monad
  import cats.instances.list._
  val monadList = Monad[List] // fetch the implicit Monad[List]
  val aSimpleList = monadList.pure(2) // List(2)
  val anExtendedList = monadList.flatMap(aSimpleList)(x => List(x, x + 1))

  // applicable to Option, Try, Future

  // either is also a monad
  val aManualEither: Either[String, Int] = Right(42)

  type LoadingOr[T] = Either[String, T]
  type ErrorOr[T] = Either[Throwable, T]

  import cats.instances.either._
  val loadingMonad = Monad[LoadingOr]
  val anEither = loadingMonad.pure(45) // LoadingOr[Int] = Right(45)
  val aChangedLoading = loadingMonad.flatMap(anEither)(n =>
    if (n % 2 == 0) Right(n + 1) else Left("Loading meaning of life...")
  )

  // let's say I have an imaginary online store
  case class OrderStatus(orderId: Long, status: String)
  def getOrderStatus(orderId: Long): LoadingOr[OrderStatus] = Right(
    OrderStatus(orderId, "Ready to ship")
  )
  def trackLocation(orderStatus: OrderStatus): LoadingOr[String] =
    if (orderStatus.orderId > 1000)
      Left("Not available yet, refreshing data...")
    else Right("Amsterdam, NL")

  val orderId = 457L
  val orderLocation =
    loadingMonad.flatMap(getOrderStatus(orderId))(trackLocation)

  // use extension methods
  import cats.syntax.flatMap._
  import cats.syntax.functor._
  val orderLocationBetter: LoadingOr[String] =
    getOrderStatus(orderId).flatMap(trackLocation)

  val orderLocationFor: LoadingOr[String] = for {
    orderStatus <- getOrderStatus(orderId)
    location <- trackLocation(orderStatus)
  } yield location

  // TODO: The service layer API of a web app
  case class Connection(host: String, port: String)
  val config = Map(
    "host" -> "localhost",
    "port" -> "4040"
  )

  trait HttpService[M[_]] {
    def getConnection(cfg: Map[String, String]): M[Connection]
    def issueRequest(connection: Connection, payload: String): M[String]
  }
  // DO NOT CHANGE THE CODE

  /*
    Requirements:
      - if the host and port are found in the configuration map, then we'll return a M containing a connection with
        those values. Otherwise the method will fail, according to the logic of the type M
      - the issueRequest method returns a M containing the string: "request (payload) has been accepted", if the payload
        is less than 20 characters. Otherwise, the method will fail, according to the logic of the type M
   */

  // The exercise will consist of implementing the HttpService in the form of try, option, future, or either
  type EitherOr[T] = Either[String, T]

  object EitherHttpService extends HttpService[EitherOr] {

    override def getConnection(
        cfg: Map[String, String]
    ): EitherOr[Connection] = {
      val connectionConfig = for {
        host <- cfg.get("host")
        port <- cfg.get("port")
      } yield Connection(host, port)

      connectionConfig match {
        case Some(connection) => Right(connection)
        case None => Left("Host or Port are missing in the configuration")
      }
    }

    override def issueRequest(
        connection: Connection,
        payload: String
    ): EitherOr[String] = {
      if (payload.size < 20)
        Right(s"request '$payload' has been accepted")
      else
        Left("Request has failed to be issued. Payload size is < 20 characters")
    }

  }

  val responseEither = for {
    conn <- EitherHttpService.getConnection(config)
    response <- EitherHttpService.issueRequest(conn, "Hello, HTTP service")
  } yield response

  println(responseEither)

  // we can now leverage previous for comprehension to a generic method that receives a Monad:
  def getResponse[M[_]](service: HttpService[M], payload: String)(implicit
      monad: Monad[M]
  ): M[_] = for {
    conn <- service.getConnection(config)
    response <- service.issueRequest(conn, payload)
  } yield response

  println(getResponse(EitherHttpService, "Monad service"))
}
