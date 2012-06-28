package org.draegisoft.euler

object Euler004 {
  def euler(): Unit = {
    val start = System.currentTimeMillis
    val largest = largestPalindrome(3)
    val time = System.currentTimeMillis - start
    println("4th problem: %d and required %d ms".format(largest, time))
  }

  def largestPalindrome(digits: Int): Int = {
    val upper = math.pow(10, digits).toInt
    val lower = math.pow(10, digits -1).toInt
    var maxPalindrome = 0
    for (i <- (lower until upper).reverse)
      for (j <- (lower until upper).reverse){
        val candidate = i * j
        if (isPalindrome(candidate))
          maxPalindrome = math.max(maxPalindrome, candidate)
      }
    maxPalindrome  
  }

  def isPalindrome(value: Long) = {
    val s: String = value.toString
    s == s.reverse
  }
}
