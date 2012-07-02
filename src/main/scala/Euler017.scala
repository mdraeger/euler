package org.draegisoft.euler

object Euler017 {
  def euler(): Unit ={
    val start = System.currentTimeMillis
    val numberOfChars = numberOfCharacters(1 to 1000)
    val time = System.currentTimeMillis - start
    println("17th problem: %d required %d ms.".format(numberOfChars, time))
  }

  def numberOfCharacters(range: Range): Int = {
    val numbersAsWords = range map (number => numberAsWord(number))
    (0 /: (numbersAsWords map (word => word.replaceAll(" ", "").replaceAll("\\-", "").length))) (_ + _)
  }

  def numberAsWord(number: Int): String = {
    var tmpNum = number
    var exponent = 0
    val sb = new StringBuilder()
    while (tmpNum > 0){
      val remainder = tmpNum % 10
      if (exponent == 1 && number % 100 < 20){
        sb.clear()
        sb.insert(0, digitAsWord(number % 100))
      } else if (exponent == 1 && remainder > 0){
        sb.insert (0, digitAsWord(remainder * 10) + "-")
      } else if (exponent == 2 && remainder > 0) {
        sb.insert (0, digitAsWord(remainder) + " " + digitAsWord(100) + " and ")
    } else if (exponent == 3 && remainder > 0) {
        sb.insert (0, digitAsWord(remainder) + " " + digitAsWord(1000) + " " )
    } else {
        sb.insert (0, digitAsWord(remainder))
      }
      tmpNum /= 10
      exponent += 1
    }
    if (sb.toString.endsWith(" and "))
      sb.delete (sb.length - 5, sb.length)
    sb.toString.trim
  }

  def digitAsWord(digit: Int): String = {
    digit match {
        case 1 => "one" 
        case 2 => "two" 
        case 3 => "three" 
        case 4 => "four" 
        case 5 => "five" 
        case 6 => "six" 
        case 7 => "seven" 
        case 8 => "eight" 
        case 9 => "nine" 
        case 10 => "ten" 
        case 11 => "eleven" 
        case 12 => "twelve" 
        case 13 => "thirteen" 
        case 14 => "fourteen" 
        case 15 => "fifteen" 
        case 16 => "sixteen" 
        case 17 => "seventeen" 
        case 18 => "eighteen" 
        case 19 => "nineteen" 
        case 20 => "twenty" 
        case 30 => "thirty" 
        case 40 => "forty" 
        case 50 => "fifty" 
        case 60 => "sixty" 
        case 70 => "seventy" 
        case 80 => "eighty" 
        case 90 => "ninety" 
        case 100 => "hundred" 
        case 1000 => "thousand" 
        case _ => ""
    }
  }
}
