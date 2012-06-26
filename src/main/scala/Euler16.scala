package org.draegisoft.euler

object Euler16 {
  def euler(): Unit = {
    val start = System.currentTimeMillis
    val sum = sumDigits(BigInt(2).pow(1000))
    val time = System.currentTimeMillis - start
    println("16th problem: %d and required %d ms".format(sum, time))
  }

  def sumDigits(num: BigInt): Int = {
    val digitsAsInts = num.toString.map (d => Integer.parseInt(""+d))
    (0 /: digitsAsInts) (_ + _)
  }
}
