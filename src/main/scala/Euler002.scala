package org.draegisoft.euler

object Euler002 {
  def euler(): Unit = {
    val start = System.currentTimeMillis
    val sum = sumOfEvenValuedTerms(4000000)
    val time = System.currentTimeMillis - start
    println("2nd problem: %d and required %d ms".format(sum, time))
  }

  def sumOfEvenValuedTerms(upperBound: Int): Int = {
    lazy val fibonacci: Stream[Int] = 0 #:: 1 #:: (fibonacci zip fibonacci.tail).map{
      case (a,b) => a+b
    }
    (0 /: fibonacci.filter(_ % 2 == 0).takeWhile(_ <= upperBound)) (_ + _)
  }
}
