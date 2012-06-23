package org.draegisoft.euler.test

import collection.immutable.Seq

import org.draegisoft.euler._

import org.scalatest.FunSuite

class EulerSuite extends FunSuite {
  test("1st problem: the natural numbers below 10 which are multiples of 3 or 5 add up to 23: " ){
    assert (Euler1.euler(Seq(3,5), 10) === 23)
  }
  
  test("2nd problem: the even values below 100 of a Fibonacci series add up to 44: " ){
    assert (Euler2.euler(100) === 44)
  }
  
  test("3rd problem: the largest prime factor of 13195 is 29: " ){
    assert (Euler3.euler(13195) === 29)
  }
  
  test("4th problem: largest palindromic number that is a multiple of two two-digit numbers is 9009: " ){
    assert (Euler4.euler(2) === 9009)
  }
  
  test("5th problem: the smallest number that can be divided by each of the numbers from 1 to ten is 2520: " ){
    assert (Euler5.euler(1, 10) === 2520)
  }
  
  test("6th problem: the difference between the sum of squares and the square of the sum of 1 to 10 is 2640: " ){
    assert (Euler6.euler(1, 10) === 2640)
  }
  
  test("7th problem: the 6th prime number is 13: " ){
    assert (Euler7.euler(6) === 13)
  }
  
  test("8th problem: the greatest product of five consecutive numbers in 123451111 is 120: " ){
    assert (Euler8.euler("123451111", 5) === 120)
  }
  
  test("9th problem: : the Pythagorean triple with sum 12 (3,4,5) has the product 60: " ){
    assert (Euler9.euler(12) === 60)
  }
  
  test("10th problem: : the sum of all the primes below 10 is 17: " ){
    assert (Euler10.euler(10) === 17)
  }
  
  test("11th problem: : max product of four adjacent numbers is 70600674: " ){
    assert (Euler11.euler === 70600674)
  }

  test("12th problem: : smallest triangle number with more than 5 divisors is 28: " ){
    assert (Euler12.euler(5) === 28)
  }

  test("13th problem: add a number block: " ){
    assert (Euler13.euler("1000000000\n1") === "1000000001")
  }

  test("14th problem: the Collatz sequence starting at 13 has 10 terms"){
    assert (Euler14.collatzLength(13) === 10)
  }
}
