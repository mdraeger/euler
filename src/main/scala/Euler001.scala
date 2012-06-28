package org.draegisoft.euler

import collection.immutable.Seq

object Euler001 {
  def euler(): Unit = {
    val start = System.currentTimeMillis
    val sum = sumOfMultiples(Seq(3,5), 1000)
    val time = System.currentTimeMillis - start
    println("1st problem: %d and required %d ms".format(sum, time))
  }

  def sumOfMultiples(roots: Seq[Int], upperBound: Int): Int = {
    val numbers = (1 until upperBound).filter(i => {
      (roots filter (i % _ == 0)).size >= 1
    })
    (0 /: numbers) (_ + _)
  }
}
