package org.draegisoft.euler

object Euler6 {
  def euler(lower: Int, upper: Int): Int = {
    val numbers = lower to upper
    val sumOfSquares = (0 /: (numbers map (x => x*x))) (_ + _)
    val sum = (0 /: numbers) (_ + _)
    sum * sum - sumOfSquares
  }
}
