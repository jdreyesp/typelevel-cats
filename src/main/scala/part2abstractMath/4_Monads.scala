package part2abstractMath

import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors
import scala.concurrent.Future

object Monads extends App {

  val numbersList = List(1, 2, 3)
  val charsList = List('a', 'b', 'c')

  // TODO 1.1 how do you create all combinations of (number, char)?
  val combinationsList = numbersList.flatMap(n => charsList.map(c => (n, c)))
  val combinationsListFor = for {
    n <- numbersList
    c <- charsList
  } yield (n, c)

  // options
  val numberOption = Option(2)
  val charOption = Option('d')
  // TODO 1.2: how do you create the combination of (number, char)?
  val combinationsOption =
    numberOption.flatMap(n => charOption.map(c => (n, c)))
  val combinationsOptionFor = for {
    n <- numberOption
    c <- charOption
  } yield (n, c)

  // futures
  val executor = Executors.newFixedThreadPool(8)
  implicit val ec: ExecutionContext =
    ExecutionContext.fromExecutorService(executor)
  val numberFuture = Future(42)
  val charFuture = Future('Z')
  // TODO: 1.3: how do you create the combination of (number, char)?
  val combinationsFuture =
    numberFuture.flatMap(n => charFuture.map(c => (n, c)))
  val combinationsFutureFor: Future[(Int, Char)] = for {
    n <- numberFuture
    c <- charFuture
  } yield (n, c)

  combinationsFuture.onComplete(_ => println("finished"))

  /** Pattern:
    *   - wrapping a value into an M (monadic) value
    *   - the flatMap mechanism
    *
    * MONAD
    */
  trait MyMonad[M[_]] {
    def pure[A](value: A): M[A] // wrapping a value into an M value
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
    // map also belongs to Monad, but the pattern is only about pure and flatMap.
    // We can then say that Monad extends a Functor (see the definition: Monad <: Applicative <: Apply <: Functor)
    def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => pure(f(a)))
  }

  // Cats monad
  import cats.Monad
  import cats.instances.option._

  val optionMonad = Monad[Option]
  val anOption = optionMonad.pure(4) // Option(4) == Some(4)
  val aTransformedOption =
    optionMonad.flatMap(anOption)(x => if (x % 3 == 0) Some(x + 1) else None)

  import cats.instances.list._
  val listMonad = Monad[List]
  val aList = listMonad.pure(3) // List[Int](3)
  val aTransformedList =
    listMonad.flatMap(aList)(x => List(x, x + 1)) // List(4, 5)

  // TODO: use a Monad[Future]
  import cats.instances.future._
  val futureMonad = Monad[Future]
  val aFuture = futureMonad.pure(3)
  val aTransformedFuture = futureMonad.flatMap(aFuture)(x => Future(x + 1))

  // let's print the futures
  val combinedFutures = for {
    combined <- combinationsFuture
    value <- aTransformedFuture
  } yield (combined, value)

  combinedFutures.onComplete(println)

  // specialized API
  def getPairsList(numbers: List[Int], chars: List[Char]): List[(Int, Char)] =
    numbers.flatMap(n => chars.map(c => (n, c)))
  def getPairsOption(
      number: Option[Int],
      char: Option[Char]
  ): Option[(Int, Char)] =
    number.flatMap(n => char.map(c => (n, c)))

  // with monads, we generalize this API
  def getPairs[M[_], A, B](ma: M[A], mb: M[B])(implicit
      monad: Monad[M]
  ): M[(A, B)] =
    monad.flatMap(ma)(a => monad.map(mb)(b => (a, b))) // we can use map too.

  // we see now how powerful thi sis
  println(getPairs(numbersList, charsList))
  println(getPairs(numberOption, charOption))
  getPairs(numberFuture, charFuture).foreach(println)

  // extension methods - pure, flatMap
  import cats.syntax.applicative._ // pure is here (applicative is kinda a weaker version of monad)
  val oneOption =
    1.pure[Option] // Some(1) =>  implicit Monad[Option] will be use
  val oneList = 1.pure[List]

  import cats.syntax.flatMap._ // flatMap is here
  val oneOptionTransformed = oneOption.flatMap(x => (x + 1).pure[Option])

  // TODO 3: Using map function
  // two ways: Using the Monad map
  val oneOptionMapped = Monad[Option].map(Option(2))(_ + 1)

  // or use the functor Map (since Monad is a Functor)
  import cats.syntax.functor._ // map is here
  val oneOptionMapped2 = oneOption.map(_ + 2)

  // for-comprehensions
  val composedOptionFor = for {
    one <- 1.pure[Option]
    two <- 2.pure[Option]
  } yield one + two

  // TODO 4: implement a shorter version of getPairs using for-comprehensions
  def getPairs_v2[M[_], A, B](ma: M[A], mb: M[B])(implicit
      monad: Monad[M]
  ): M[(A, B)] =
    for {
      a <- ma // Monad provides the flatMap!
      b <- mb
    } yield (a, b)

  println(getPairs_v2(List(1, 2, 3), List('a', 'b', 'c')))

  Thread.sleep(1000)
  executor.shutdown()
}
