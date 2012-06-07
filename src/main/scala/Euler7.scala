
package org.draegisoft.euler

object Euler7 {
  def euler(index: Int): Long = {
    (primes take (index)) last
  }
}
