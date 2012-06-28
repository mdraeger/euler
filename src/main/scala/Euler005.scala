package org.draegisoft.euler

object Euler005 {
  def euler(): Unit = {
    val start = System.currentTimeMillis
    val lcmFrom1To20 = lcmOfRange(1 to 20)
    val time = System.currentTimeMillis - start
    println("5th problem: %d and required %d ms".format(lcmFrom1To20, time))
  }

  def lcmOfRange(range: Range): Long = {
    (1L /: (range)) (lcm(_, _))
  }
}
