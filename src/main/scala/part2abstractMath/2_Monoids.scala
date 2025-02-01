package part2abstractMath

object Monoids extends App {

  import cats.Semigroup
  import cats.instances.int._
  import cats.syntax.semigroup._ // Import the |+| extension method
  val numbers = (1 to 1000).toList

  // |+| is always associative
  val sumLeft = numbers.foldLeft(0)(_ |+| _)
  val sumRight = numbers.foldRight(0)(_ |+| _)

  println(sumLeft)
  println(sumRight)

  // define a general API
  // def combineFold[T](list: List[T])(implicit semigroup: Semigroup[T]): T =
  // list.foldLeft()(_ |+| _) // what's the starting value? Semigroup is not enough

  // MONOIDS - same as semigroups but with the ability of providing an empty value
  import cats.Monoid
  val intMonoid = Monoid[Int]
  val combineInt = intMonoid.combine(23, 999) // 1022
  val zero = intMonoid.empty // 0

  import cats.instances.string._ // bring the implicit Monoid[String] in scope
  val emptyString = Monoid[String].empty // ""
  val combineString = Monoid[String].combine("I understand ", "monoids")

  import cats.instances.option._
  val emptyOption = Monoid[Option[Int]].empty // None
  val combineOption =
    Monoid[Option[Int]].combine(Option(2), Option.empty[Int]) // Some(2)

  // extension method for Monoids - |+|
  // no need to import since we already have import cats.syntax.semigroup._
  // same could be with import cats.syntax.monoid._
  val combinedOptionFancy = Option(3) |+| Option(7)

  // TODO 1: Implement a combineFold
  def combineFold[T](list: List[T])(implicit monoid: Monoid[T]): T =
    list.foldLeft(Monoid[T].empty)(_ |+| _)

  println(combineFold(List(1, 2, 3)))
}
