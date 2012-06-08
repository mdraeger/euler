package org.draegisoft.euler

object Euler9 {
  def euler(sum: Int): Int = {
    var res = 0
    for (a <- 1 until sum/2){
      for (b <- 1 until (sum - a)){
        val c = sum - a - b
        if (a*a + b*b == c*c){
          res = a*b*c
          return res
        }
      }
    }
    res
  }
}
