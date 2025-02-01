package part2abstractMath

object Semigroups extends App {

  // Semigroups combine elements of the same type
  import cats.Semigroup
  import cats.instances.int._

  val naturalIntSemigroup = Semigroup[Int]
  val intCombination = naturalIntSemigroup.combine(2, 46) // addition

  import cats.instances.string._
  val naturalStringSemigroup = Semigroup[String]
  val stringCombination =
    naturalStringSemigroup.combine("I love ", "Cats") // concatenation
  println(intCombination)
  println(stringCombination)

  def reduceInts(list: List[Int]): Int =
    list.reduce(
      naturalIntSemigroup.combine
    ) // since combine is a function that takes (x,y) -> y

  // e.g
  val numbers = (1 to 10).toList
  println(reduceInts(numbers))

  def reduceStrings(list: List[String]): String =
    list.reduce(
      naturalStringSemigroup.combine
    ) // since combine is a function that takes (x,y) -> y

  val strings = (1 to 10).map(_ => " hi ").toList
  println(reduceStrings(strings))

  // we identify the pattern, and we can now declare:
  def reduceThings[T](list: List[T])(implicit semigroup: Semigroup[T]): T =
    list.reduce(semigroup.combine)
  // now we can reduce whatever is declared as a semigroup, and we will reduce it!

  println(reduceThings(numbers))
  println(reduceThings(strings))

  import cats.instances.option._ // compiler will produce an implicit Semigroup[Option[Int ]]
  val numberOptions: List[Option[Int]] = numbers.map(n => Option(n))
  println(
    reduceThings(numberOptions)
  ) // an Option[Int] containing the sum of all the numbers

  // TODO 1: Support a new type
  case class Expense(id: Long, amount: Double)
  implicit val expenseSemigroup: Semigroup[Expense] = Semigroup.instance {
    (e1: Expense, e2: Expense) =>
      Expense(Math.max(e1.id, e2.id), e1.amount + e2.amount)
  }

  // Extension methods for semigroup: |+|
  import cats.syntax.semigroup._
  val anIntSum = 2 |+| 3
  val aStringConcat = "we like " |+| "semigroups"
  val aCombinedExpense = Expense(4, 80) |+| Expense(56, 46)

  // TODO 2: Implement reduceThings2
  // Note that we use Scala's feature 'context bound' ([T: Semigroup]),
  // forcing the method to expect an implicit value of Semigroup
  def reduceThings2[T: Semigroup](list: List[T]): T = list.reduce(_ |+| _)

  println(reduceThings2(List[Int](1, 2, 3)))
}
