package org.draegisoft.euler

import collection.immutable.Seq

object Euler2 {
  def euler2(upperBound: Int): Int = {
    lazy val fibonacci: Stream[Int] = 0 #:: 1 #:: (fibonacci zip fibonacci.tail).map{
      case (a,b) => a+b
    }
    (0 /: fibonacci.filter(_ % 2 == 0).takeWhile(_ <= upperBound)) (_ + _)
  }
}
