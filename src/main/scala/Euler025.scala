package org.draegisoft.euler

object Euler025 {
  def euler(): Unit ={
    val start = System.currentTimeMillis
    val fib = firstFibNumOfLength(1000)
    val time = System.currentTimeMillis - start
    println("25th problem: %s required %d ms.".format(fib, time))
  }

  def firstFibNumOfLength(length: Int): String = {
    val fibs: Stream[BigInt] = BigInt(0) #:: 1 #:: ((fibs zip fibs.tail) 
        map { case (c1,c2) => c1 + c2 })
    var index = 0
    while (fibs.next.toString.length < 1000)
      index += 1
    index
  }
}
