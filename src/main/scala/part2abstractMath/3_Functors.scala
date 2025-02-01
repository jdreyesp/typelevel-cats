package part2abstractMath

import scala.util.Try

object Functors extends App {

  val aModifiedList = List(1, 2, 3).map(_ + 1) // List(2,3,4)
  val aModifiedOption = Option(2).map(_ + 1) // Some(3)
  val aModifiedTry = Try(42).map(_ + 1) // Success(43)

  // The fundamental method of a functor is `map`
  trait MyFunctor[F[_]] {
    def map[A, B](initialValue: F[A])(f: A => B): F[B]
  }

  import cats.Functor
  import cats.instances.list._ // includes Functor[List]
  val listFunctor = Functor[List]
  val incrementedNumbers = listFunctor.map(List(1, 2, 3))(_ + 1)

  import cats.instances.option._ // includes Functor[Option]
  val optionFunctor = Functor[Option]
  val incrementedOption = optionFunctor.map(Option(2))(_ + 1) // Some(3)

  import cats.instances.try_._
  val anIncrementedTry = Functor[Try].map(Try(42))(_ + 1) // Success(43)

  // If list, try, option, etc. have a map method, why do we want to bother with Functors? ->
  // Functors become important when you want to generalise a transformation

  // generalizing an API
  def do10xList(list: List[Int]): List[Int] = list.map(_ * 10)
  def do10xOption(option: Option[Int]): Option[Int] = option.map(_ * 10)
  def do10xTry(attempt: Try[Int]): Try[Int] = attempt.map(_ * 10)

  def do10x[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] =
    functor.map(container)(_ * 10)

  println(do10x(List(1, 2, 3)))
  println(do10x(Option(10)))
  println(do10x(Try(10)))

  // TODO 1: Define your own functor for a binary tree
  // hint: define an object which extends Functor[Tree]
  trait Tree[+T]
  case class Leaf[+T](value: T) extends Tree[T]
  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

  implicit object TreeFunctor extends Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Leaf(v)         => Leaf(f(v))
      case Branch(v, l, r) => Branch(f(v), map(l)(f), map(r)(f))
    }
  }

  val myTree = Branch(10, Leaf(1), Leaf(2))

  println(do10x[Tree](myTree))

  // extension method - map
  import cats.syntax.functor._
  val tree: Tree[Int] = Branch(40, Branch(5, Leaf(10), Leaf(30)), Leaf(20))
  val incrementedTree = tree.map(_ + 1)
  println(incrementedTree)
}
