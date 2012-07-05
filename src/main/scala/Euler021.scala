package org.draegisoft.euler

object Euler021 {
  def euler(): Unit ={
    val start = System.currentTimeMillis
    val sum = sumOfAmicableNumbersBelow(10000)
    val time = System.currentTimeMillis - start
    println("21th problem: %d required %d ms.".format(sum, time))
  }

  def sumOfAmicableNumbersBelow(upperLimit: Int): Int = {
    (0 /: (1 to upperLimit).filter(n => isAmicable(n))) (_ + _)
  }

  def isAmicable(a: Int): Boolean = {
    val b = d(a)
    d(b) == a && b != a
  }

  def d(n: Int): Int = {
    var sum = 1
    for(i <- 2 to (math.sqrt(n).toInt + 1))
      if (n % i == 0){
        sum += i + n/i
      }
    sum
  }
}
