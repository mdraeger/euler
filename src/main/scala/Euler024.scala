package org.draegisoft.euler

object Euler024 {
  def euler(): Unit ={
    val start = System.currentTimeMillis
    val res = (0 to 9).permutations.drop(999999).next.mkString
    val time = System.currentTimeMillis - start
    println("24rd problem: %s required %d ms.".format(res, time))
  }
}
