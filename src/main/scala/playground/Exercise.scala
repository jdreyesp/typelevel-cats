package playground

import scala.annotation.tailrec

object Exercise extends App {

  // sum all numbers from 1 to N, and store intermediate results in an accumulator

  def sum(n: Int): Int = {

    @tailrec
    def sumAcc(n: Int, acc: Int): Int =
      if (n == 0) acc
      else sumAcc(n - 1, acc + n)

    sumAcc(n, 0)
  }

  println(sum(3))
}
