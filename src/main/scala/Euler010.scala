package org.draegisoft.euler

object Euler010 {
  def euler(): Unit ={
    val start = System.currentTimeMillis
    val sum = sumOfPrimesBelow(2000000)
    val time = System.currentTimeMillis - start
    println("10th problem: %d required %d ms.".format(sum, time))
  }

  def sumOfPrimesBelow(upperLimit: Int): Long = {
    (0L /: primes.takeWhile(_ < upperLimit)) (_ + _)
  }
}
