package org.draegisoft.euler

object Euler023 {
  def euler(): Unit ={
    val start = System.currentTimeMillis
    val sum = sumOfNumbersNotSumOfAbundantNumbers(0 to 28123)
    val time = System.currentTimeMillis - start
    println("23rd problem: %d required %d ms.".format(sum, time))
  }

  def sumOfNumbersNotSumOfAbundantNumbers(range: Range): Int = {
    val allNumbers = range.toArray
    val abundantNumbers = listOfAbundantNumbers(12 to range.max)
    for (i <- 0 until abundantNumbers.length){
      for (j <- 0 until abundantNumbers.length){
        val sum = abundantNumbers(i) + abundantNumbers(j)
        if (sum < allNumbers.length)
          allNumbers(sum) = 0
      }
    }

    allNumbers.sum
  }


  def listOfAbundantNumbers(range: Range): Array[Int] = {
    range.filter(n => isAbundant(n)).toArray
  }

  def isAbundant(n: Int) = sumOfProperDivisors(n) > n

}
