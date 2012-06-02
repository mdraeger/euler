package org.draegisoft.euler

import collection.immutable.Seq

object Euler1 {
  def euler1(roots: Seq[Int], upperBound: Int): Int = {
    val numbers = (1 until 10).filter(i => {
      (roots filter (i % _ == 0)).size >= 1
    })
    (0 /: numbers) (_ + _)
  }
}
