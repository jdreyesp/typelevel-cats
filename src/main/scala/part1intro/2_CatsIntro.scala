package part1intro

object CatsIntro extends App {

  // Eq
  val aComparison =
    2 == "a string" // This comparison does not compare the types... Eq to the rescue

  // same 4 parts as the type classes object:

  // part1 - type class import
  import cats.Eq

  // part2 - import Type Class instances for the types you need
  import cats.instances.int._

  // part 3 - use the TC API
  val intEquality = Eq[Int]
  val aTypeSafeComparison = intEquality.eqv(2, 3) // false
  // val anUnsafeComparison = intEquality.eqv(2, "A") // does not compile!

  // part 4 - use extension methods (if applicable)
  import cats.syntax.eq._ // syntax contains all the extension methods
  val anotherTypeSafeComp = 2 === 3 // false
  val neqcomparison = 2 =!= 3 // true
  // val invalidComparison = 2 === "hello!" // it does not compile
  // extension methods are only visible in the presence of the right TC instance

  // part 5 - extending the TC operations to composite types, e.g. lists
  import cats.instances.list._ // we bring Eq[List[Int]] in scope
  val aListComparison = List(2) === List(3) // valid and it returns false

  // part 6 - what if the type is not supported by cats? We create a new TC instance for a custom type
  case class ToyCar(model: String, price: Double)
  implicit val toyCarEq: Eq[ToyCar] = Eq.instance[ToyCar] { (car1, car2) =>
    car1.price == car2.price
  }

  val compareTwoToyCars =
    ToyCar("Ferrari", 29.99) === ToyCar(
      "Lamborghini",
      29.99
    ) // This will return true
  println(compareTwoToyCars)
}
