package org.draegisoft.euler

object Euler009 {
  def euler(): Unit ={
    val start = System.currentTimeMillis
    val tripleProduct = pythagoreanTripleProduct(1000)
    val time = System.currentTimeMillis - start
    println("9th problem: %d required %d ms.".format(tripleProduct, time))
  }

  def pythagoreanTripleProduct(sum: Int): Int = {
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
