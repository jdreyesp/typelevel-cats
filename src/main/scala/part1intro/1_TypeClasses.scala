package part1intro

object TypeClasses extends App {

  case class Person(name: String, age: Int)

  // part1 - type class definition
  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  // part2 - create the implicit type class instance
  implicit object StringSerializer extends JSONSerializer[String] {
    override def toJson(value: String): String = "\"" + value + "\""
  }

  implicit object IntSerializer extends JSONSerializer[Int] {
    override def toJson(value: Int): String = value.toString()
  }

  implicit object PersonSerializer extends JSONSerializer[Person] {
    override def toJson(value: Person): String = s"""
      | { "name": ${value.name}, "age": ${value.age} }
    """.stripMargin.trim
  }

  // part 3 - offer some API
  def convertListToJSON[T](list: List[T])(implicit
      serializer: JSONSerializer[T]
  ): String =
    list.map(v => serializer.toJson(v)).mkString("[", ",", "]")

  // part 4 - extending the existing types via extension methods
  object JsonSyntax {
    // this can be replaced with the `extension` word in Scala3
    implicit class JSONSerializable[T](value: T)(implicit
        serializer: JSONSerializer[T]
    ) {
      def toJson: String = serializer.toJson(value)
    }
  }

  // with this, only steps 1 to 3 are really required
  val alice = Person("Alice", 23)
  val xavier = Person("Xavier", 45)
  println(convertListToJSON(List(alice, xavier)))

  // usage of step 4
  import JsonSyntax._
  println(s"Alice's json: ${alice.toJson}")
}
