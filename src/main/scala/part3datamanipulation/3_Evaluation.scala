package part3datamanipulation

object Evaluation extends App {

  /** Cats makes the distinction between
    *   - evaluating an expression eagerly
    *   - evaluating lazily and every time you request it
    *   - evaluating lazily and keeping the value (memoizing)
    */
  import cats.Eval

  // - evaluating an expression eagerly
  val instantEval = Eval.now { // eagerly evaluated
    println("Computing now!")
    52342
  }

  // at this moment "Computing now" is printed.

  // If I want to see the value that's stored in the eval:
  println(instantEval.value)

  // - evaluating lazily and every time you request it
  val redoEval = Eval.always {
    print("Computing again: ")
    4323
  }

  // No output!

  // If I want to evaluate it:
  println(redoEval.value)
  println(
    redoEval.value
  ) // of course this repeats the evaluation since it's not memoized

  // - evaluating lazily and keeping the value (memoizing)
  val delayedEval = Eval.later {
    println("Computing later!")
    51241
  }

  println(delayedEval.value)
  println(delayedEval.value) // prints only once! (it's memoized)

  /// Benefit of Eval is the composability (in a functional way) of the expressions that you want to evaluate
  println("\n\n")
  val instantEval2 = Eval.now { // eagerly evaluated
    println("Computing now!")
    52342
  }
  val delayedEval2 = Eval.later {
    println("Computing later!")
    51241
  }

  val composedEvaluation =
    instantEval2.flatMap(value1 => delayedEval2.map(value2 => value1 + value2))

  println(composedEvaluation.value)

  // we can memoize manually
  val memoizedInstantEval = instantEval.memoize
}
