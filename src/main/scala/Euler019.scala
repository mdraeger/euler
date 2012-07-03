package org.draegisoft.euler

object Euler019 {
  def euler(): Unit ={
    val start = System.currentTimeMillis
    val sum = numOfSundaysOnTheFirstOfMonth((1, 1, 1901), 2, (31, 12, 2000))
    val time = System.currentTimeMillis - start
    println("19th problem: %d required %d ms.".format(sum, time))
  }

  def numOfSundaysOnTheFirstOfMonth(from: (Int, Int, Int), firstDayOfWeek: Int, to: (Int, Int, Int)): Int = {
    var day = from._1
    var month = from._2
    var year = from._3
    var num = 0
    var dayOfWeek = firstDayOfWeek
    while (day <= to._1 && month <= to._2 && year <= to._3){
      day = (day % daysInMonth(month, year)) + 1
      if (day == 1){
        month = (month % 12) + 1
        if (month == 1)
          year += 1
      }
      dayOfWeek = (dayOfWeek % 7) + 1
      if (day == 1 && dayOfWeek == 7)
        num += 1
    }
    num
  }

  def isLeapYear(year: Int) = 
    (year % 4 == 0 && (year % 100 != 0)) || (year % 400 == 0)

  def daysInMonth(month: Int, year: Int) = {
    month match {
      case 2 => if (isLeapYear(year)) 29 else 28
      case 1|3|5|7|8|10|12 => 31
      case _ => 30
    }
  }
}
