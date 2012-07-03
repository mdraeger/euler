package org.draegisoft.euler

object Euler020 {
  def euler(): Unit ={
    val start = System.currentTimeMillis
    val sum = sumOfDigits(factorial(BigInt(100)))
    val time = System.currentTimeMillis - start
    println("20th problem: %d required %d ms.".format(sum, time))
  }

  def sumOfDigits(n: BigInt) = 
    (0 /: n.toString.map(d => Integer.parseInt(d+"")))(_+_)

  def factorial(n: BigInt): BigInt = 
    if (n==BigInt(0))
      BigInt(1)
    else 
       n * factorial(n-1)
}
