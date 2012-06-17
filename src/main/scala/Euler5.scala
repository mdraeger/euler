package org.draegisoft.euler

object Euler5 {
  def euler(lower: Long, upper: Long): Long = {
    (1L /: (lower to upper)) (lcm(_, _))
  }
}
