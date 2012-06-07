package org.draegisoft.euler

import collection.mutable.Seq

object Euler5 {
  def euler(lower: Int, upper: Int): Long = {
    (1 /: (lower to upper)) (lcm(_, _))
  }
}
