package org.draegisoft.euler

object Euler059 {
  def euler(): Unit ={
    val start = System.currentTimeMillis
    val sum = sumOfBytes("cipher.txt")
    val time = System.currentTimeMillis - start
    println("59th problem: %d required %d ms.".format(sum, time))
  }

  def sumOfBytes(filename: String): Int = {
    0
  }
}
