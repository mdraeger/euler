package org.draegisoft.euler

import Array._

object Euler15 {
  def euler(xDim: Int, yDim: Int): Long = {
    val grid = ofDim[Long](xDim+1, yDim+1)
    grid(xDim)(yDim) = 1L
    for (i <- (0 to xDim).reverse){
      for (j <- (0 to yDim).reverse){
        if (i < xDim || j < yDim)
          grid(i)(j) = 0L 
        if (j < yDim)
          grid(i)(j) += grid(i)(j+1)
        if (i < xDim)
          grid(i)(j) += grid(i+1)(j)
      }
    }
    grid(0)(0)
  }

}
