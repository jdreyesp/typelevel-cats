package part2abstractMath

import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors
import scala.concurrent.Future

object MonadTransformers extends App {

  // sometimes we need to operate with combinations of monadic values (e.g. List[Option]) and nest them together

  def sumAllOptions(values: List[Option[Int]]): Int = ???

  // in order to combine all elements of the list, we would need to unwrap, combine and wrap all options inside the list
  // monad transformers achieve combining these kind of combined values.

  import cats.data.OptionT // option transformer
  import cats.instances.list._

  // OptionT[List, Int] represents a List[Option[Int]] (you read it -> middle, left, right :) )
  val listOfNumberOptions: OptionT[List, Int] = OptionT(
    List(Option(1), Option(2))
  )
  val listOfCharOptions: OptionT[List, Char] = OptionT(
    List(Option('a'), Option('b'), Option.empty[Char])
  )
  val listOfTuples: OptionT[List, (Int, Char)] = for {
    char <- listOfCharOptions
    number <- listOfNumberOptions
  } yield (number, char)

  println(listOfTuples.value)

  // Another interesting one: Either transformer
  import cats.data.EitherT

  val listOfEithers: EitherT[List, String, Int] = EitherT(
    List(Left("something wrong"), Right(43), Right(2))
  )
  val executorService = Executors.newFixedThreadPool(8)
  implicit val ec: ExecutionContext =
    ExecutionContext.fromExecutorService(executorService)

  val futureOfEither: EitherT[Future, String, Int] = EitherT(
    Future(Right(45).asInstanceOf[Either[String, Int]])
  )

  // TODO exercise
  // we have a multi-machine cluster for your business that will receive a traffic surge following a media appearance
  // we measure bandwith in units
  // we want to allocate TWO of our servers to cope with the traffic spike
  // we know the current capacity for each server and we know we'll hold the traffic if the sum of bandwith is > 250
  import cats.instances.future._

  val bandwiths = Map(
    "server1.rockthejvm.com" -> 50,
    "server2.rockthejvm.com" -> 300,
    "server3.rockthejvm.com" -> 170
  )

  type AsyncResponse[T] = EitherT[Future, String, T]

  def getBandwith(serverName: String): AsyncResponse[Int] = {

    bandwiths.get(serverName) match {
      case None =>
        EitherT.left(
          Future(
            s"Server $serverName unreachable"
          )
        )
      case Some(b) =>
        EitherT.right(Future(b))
    }
  }

  // can the servers withstand the traffic spike?
  // hint: call getBandwith twice, and combine the results
  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] = for {
    bandWith1 <- getBandwith(s1)
    bandWith2 <- getBandwith(s2)
  } yield (bandWith1 + bandWith2 >= 250)

  // this will return s"$s1 and $s2 can cope with the traffic spike" if the servers can withstand the surge. If not, it will contain a left with the why
  // hint: call canWithstandSurge + transform
  def generateTrafficSpikeReport(
      s1: String,
      s2: String
  ): AsyncResponse[String] = canWithstandSurge(s1, s2).transform(either =>
    either match {
      case Left(value) => Left(value)
      case Right(false) =>
        Left(s"Warning: $s1 and $s2 can't cope with the traffic spike")
      case Right(true) =>
        Right(s"$s1 and $s2 can cope with the traffic spike")
    }
  )

  generateTrafficSpikeReport(
    "server1.rockthejvm.com",
    "server2.rockthejvm.com"
  ).value.foreach(println)

  generateTrafficSpikeReport(
    "server1.rockthejvm.com",
    "server3.rockthejvm.com"
  ).value.foreach(println)

  generateTrafficSpikeReport(
    "server1.rockthejvm.com",
    "server5.rockthejvm.com"
  ).value.foreach(println)

  Thread.sleep(1000)
  executorService.shutdown()
}
