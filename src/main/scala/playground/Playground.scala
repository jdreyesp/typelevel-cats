package playground

import cats.Eval

object Playground extends App {

  val meaningOfLife = Eval.later {
    println("Learning cats: computing abstractions and the meaning of life...")
    42
  }

  println(meaningOfLife.value)
}
