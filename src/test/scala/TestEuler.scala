package org.draegisoft.euler.test

import collection.immutable.Seq

import org.draegisoft.euler._

import org.scalatest.FunSuite

class EulerSuite extends FunSuite {
  test("1st problem: the natural numbers below 10 which are multiples of 3 or 5 add up to 23: " ){
    assert (Euler1.euler1(Seq(3,5), 10) === 23)
  }
  
  test("2nd problem: the even values below 100 of a Fibonacci series add up to 44: " ){
    assert (Euler2.euler2(100) === 44)
  }
  
  test("3rd problem: the largest prime factor of 13195 is 29: " ){
    assert (Euler3.euler3(13195) === 29)
  }
}
