package part3datamanipulation

object Writers extends App {
  import cats.data.Writer

  // The writer is a wraper over a value. As you transform the data, you want to save some sort of information (like logs, or other things)
  val aWriter: Writer[List[String], Int] = Writer(List("Started something"), 45)

  // the point of the writer is to define one at the beginning of your application and then you manipulate them (in a functional way)
  // so that you keep it for the whole lifecycle of your application

  // mapping
  val anIncreasedWriter =
    aWriter.map(_ + 1) // value increases, logs stay the same
  val aLogsWriter = aWriter.mapWritten(
    _ :+ "found something interesting"
  ) // value stays the same, logs change
  val aWriterWithBoth =
    aWriter.bimap(_ :+ "found something interesting", _ + 1) // both change
  val aWriterWithBoth2 = aWriter.mapBoth((logs, value) =>
    (logs :+ "found something interesting", value + 1)
  )

  // eligible for for comprehensions
  val writerA = Writer(Vector("Log A1", "Log A2"), 10)
  val writerB = Writer(Vector("Log B1"), 40)

  // Under the hood this uses WriterT (which is a monad transformer WriterT[Id,Vector[String],Int]) and its flatMap capability
  // However, compiler complains about a semigroup not existing, and that is because the Monad transformer needs to know how to combine elements of the vector
  // So if I import this:
  import cats.instances.vector._
  // It will use the natural combinator of the vector (the semigroup of vector) which will apply concatenation of the strings as the combinator.

  val compositeWriter = for {
    va <- writerA
    vb <- writerB
  } yield va + vb

  println(compositeWriter.run)

  // reset the logs
  import cats.instances.list._ // for an implicit Monoid[List[String]]
  val emptyWriter = aWriter.reset // clear the logs, keep the value

  // Dump either the value of the logs
  val desiredValue = aWriter.value
  val logs = aWriter.written
  val (l, v) = aWriter.run
}
