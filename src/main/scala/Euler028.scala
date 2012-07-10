package org.draegisoft.euler

object Euler028 {
  def euler(): Unit ={
    val start = System.currentTimeMillis
    val sum = sumOfCorners(1001)
    val time = System.currentTimeMillis - start
    println("28th problem: %d required %d ms.".format(sum, time))
  }

  def sumOfCorners(dim: Int): Int = {
    val distances = (0 until 2*(dim - 1)).toArray.map(n => 2*(n/4) + 2)
    val cornerVals = Array.ofDim[Int](2 * dim - 1)
    cornerVals(0) = 1
    for (i <- 1 until cornerVals.length)
      cornerVals(i) = cornerVals(i-1) + distances(i-1)
    cornerVals.sum
  }
}
