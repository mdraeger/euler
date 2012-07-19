package org.draegisoft.euler

object Euler021 {
  def euler(): Unit ={
    val start = System.currentTimeMillis
    val sum = sumOfAmicableNumbersBelow(10000)
    val time = System.currentTimeMillis - start
    println("21th problem: %d required %d ms.".format(sum, time))
  }

  def sumOfAmicableNumbersBelow(upperLimit: Int): Int = {
    (2 to upperLimit).filter(n => isAmicable(n)).sum
  }

  def isAmicable(a: Int): Boolean = {
    val b = sumOfProperDivisors(a)
    sumOfProperDivisors(b) == a && b != a
  }
}
