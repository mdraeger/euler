package org.draegisoft.euler

import collection.immutable.Seq

object Euler1 {
  def euler(roots: Seq[Int], upperBound: Int): Int = {
    val numbers = (1 until upperBound).filter(i => {
      (roots filter (i % _ == 0)).size >= 1
    })
    (0 /: numbers) (_ + _)
  }
}
