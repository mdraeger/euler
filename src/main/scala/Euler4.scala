package org.draegisoft.euler

object Euler4 {
  def euler(digits: Int): Long = {
    val upper = math.pow(10, digits).toInt
    val lower = math.pow(10, digits -1).toInt
    for (i <- (lower until upper).reverse)
      for (j <- (lower until upper).reverse){
        val candidate = i.toLong * j.toLong
        if (isPalindrome(candidate))
          return candidate
      }
    1L  
  }

  def isPalindrome(value: Long) = {
    val s: String = value.toString
    s == s.reverse
  }
}
