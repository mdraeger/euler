package org.draegisoft.euler

object Euler10 {
  def euler(upperLimit: Int): Long = {
    (0L /: primes.takeWhile(_ < upperLimit)) (_ + _)
  }
}
