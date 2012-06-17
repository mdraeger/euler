package org.draegisoft.euler

object Euler4 {
  def euler(digits: Int): Int = {
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
