package part1intro

object TCVariance extends App {

  import cats.Eq
  import cats.instances.int._ // Eq[Int] TC instance
  import cats.instances.option._ // construct a Eq[Option[Int]] TC instance
  import cats.syntax.eq._

  val aComparison = Option(2) === Option(3)
  // val anInvalidComparison = Some(2) === None // Eq[Some[Int]] not found

  // variance
  class Animal
  class Cat extends Animal

  // covariant type: subtying is propagated to the generic type
  class Cage[+T]
  val cage: Cage[Animal] =
    new Cage[Cat] // Cat <: Animal, so Cage[cat] <: Cage[Animal]

  // contravariant type: subtying is propagated backwards to the generic type
  class Vet[-T]
  val vet: Vet[Cat] =
    new Vet[Animal] // Cat <: Animal, then Vet[Animal] <: Vet[Cat]

  // rule of thumb: "HAS" a "T" = covariant. "ACTS" on T = contravariant
  // variance affect how TC instances are being fetched

  // CONTRAVARIANT TC
  trait SoundMaker[-T]
  implicit object AnimalSoundMaker extends SoundMaker[Animal]
  def makeSound[T](implicit soundMaker: SoundMaker[T]): Unit = println("wow")
  makeSound[Animal] // ok - TC instance defined above
  makeSound[Cat] // ok - TC instance for Animal is also applicable to Cat

  // rule 1: contravariant TCs can use the superclass instances if nothing is available strictly for that type

  // this has implications for subtypes.
  implicit object OptionSoundMaker extends SoundMaker[Option[Int]]
  makeSound[Option[Int]]
  makeSound[Some[Int]]

  // the fact that Cats' Eq is INVARIANT explains the fact that Some(3) === None does not compile

  // COVARIANT TC
  trait AnimalShow[+T] {
    def show: String
  }
  implicit object GeneralAnimalShow extends AnimalShow[Animal] {
    override def show: String = "animals everywhere"
  }

  implicit object CatsShow extends AnimalShow[Cat] {
    override def show: String = "so many cats!"
  }

  def organizeShow[T](implicit event: AnimalShow[T]): String = event.show

  println(
    organizeShow[Cat]
  ) // ok - the compiler will inject CatsShow as implicit

  println(
    //  organizeShow[Animal]
  )
  // will not compile, since both GeneralAnimalShow and CatsShow can be injected
  // (since the covariant type +T), so the compiler will complain about ambiguous implementation

  // Cats uses INVARIANT TCs just to avoid these weird situations. Just to make the initial problem solved, we should use:
  println(Option(3) === Option.empty) // false
}
