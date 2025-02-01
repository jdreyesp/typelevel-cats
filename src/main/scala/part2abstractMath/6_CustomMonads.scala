package part2abstractMath

import scala.annotation.tailrec

object CustomMonads extends App {

  import cats.Monad
  implicit object OptionMonad extends Monad[Option] {

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa.flatMap(f)

    // This is needed since every Monad needs to be 'iterated' (even though here we deal with Option)
    // This will be a tail recursive function that will return the Option[B] when the function returns
    // the right part of the Either (see implementation)
    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] =
      f(a) match {
        case None           => None
        case Some(Left(v))  => tailRecM(v)(f)
        case Some(Right(v)) => Some(v)
      }

    override def pure[A](x: A): Option[A] = Option(x)

  }

  // TODO1: Define a monad for the identity type
  type Identity[T] = T
  val aNumber: Identity[Int] = 42

  implicit object IdentityMonad extends Monad[Identity] {

    override def flatMap[A, B](fa: Identity[A])(
        f: A => Identity[B]
    ): Identity[B] = f(fa)

    @tailrec
    override def tailRecM[A, B](a: A)(
        f: A => Identity[Either[A, B]]
    ): Identity[B] = f(a) match {
      case Left(v)  => tailRecM(v)(f)
      case Right(b) => b
    }

    override def pure[A](x: A): Identity[A] = x

  }

}
