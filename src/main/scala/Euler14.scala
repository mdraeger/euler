package org.draegisoft.euler

object Euler14 {
  def euler(upper: Int): Int = {
    var best = 0
    var max = 0
    for (i <- 2 to upper){
      val length = collatzLength(i)
      if (length > max){
        best = i
        max = length
      }
    }
    best
  }
 
  def collatzLength(start: Long):Int = {
    var length = 1
    var currentValue = start
    while (currentValue != 1){
      currentValue = collatz(currentValue)
      length += 1
    }
    length
  }

  def collatz(value: Long):Long = {
    if (value % 2 == 0)
      value / 2
    else
      3 * value + 1
  }
}
