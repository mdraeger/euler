package org.draegisoft.euler

object Euler025 {

  val fibs: Stream[BigInt] = BigInt(0) #:: BigInt(1) #:: fibs.zip(fibs.tail).map { n => n._1 + n._2 }
  
  def euler(): Unit ={
    val start = System.currentTimeMillis
    val fib = firstFibNumOfLength(1000)
    val time = System.currentTimeMillis - start
    println("25th problem: %d required %d ms.".format(fib, time))
  }

  def firstFibNumOfLength(length: Int): Int = {
    var index = 0
    val fibIterator = fibs.iterator
    while (fibIterator.next.toString.length < 1000)
      index += 1
    index
  }
}
