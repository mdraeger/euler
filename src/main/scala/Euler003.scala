package org.draegisoft.euler

object Euler003 {
  def euler(): Unit = {
    val start = System.currentTimeMillis
    val largest = largestPrimeFactor(600851475143L)
    val time = System.currentTimeMillis - start
    println("3rd problem: %d and required %d ms".format(largest, time))
  }

  def largestPrimeFactor(number: Long): Long = {
    var largest = 1L
    var remainder = number
    for (prime <- primes.takeWhile(_ => remainder != 1)){
      while (remainder % prime == 0){
        largest = prime
        remainder /= prime
      }
    }
    largest
  }
}
