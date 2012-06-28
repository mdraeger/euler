package org.draegisoft.euler

object Euler006 {
  def euler(): Unit = {
    val start = System.currentTimeMillis
    val difference = squareOfSumMinusSumOfSquares(1 to 100)
    val time = System.currentTimeMillis - start
    println("6th problem: %d and required %d ms".format(difference, time))
  }

  def squareOfSumMinusSumOfSquares(range: Range): Int = {
    val sumOfSquares = (0 /: (range map (x => x*x))) (_ + _)
    val sum = (0 /: range) (_ + _)
    sum * sum - sumOfSquares
  }
}
