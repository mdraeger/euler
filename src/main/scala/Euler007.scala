package org.draegisoft.euler

object Euler007 {
  def euler(): Unit = {
    val start = System.currentTimeMillis
    val prime = indexedPrime(10001)
    val time = System.currentTimeMillis - start
    println("7th problem: %d and required %d ms".format(prime, time))
  }

  def indexedPrime(index: Int): Long = {
    (primes take (index)) last
  }
}
