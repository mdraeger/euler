package org.draegisoft.euler

object Euler3 {
  def euler3(number: Long): Long = {
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
