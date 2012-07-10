package org.draegisoft.euler.test

import collection.immutable.Seq

import org.draegisoft.euler._

import org.scalatest.FunSuite

class EulerSuite extends FunSuite {
  test("1st problem: the natural numbers below 10 which are multiples of 3 or 5 add up to 23: " ){
    assert (Euler001.sumOfMultiples(Seq(3,5), 10) === 23)
  }
  
  test("2nd problem: the even values below 100 of a Fibonacci series add up to 44: " ){
    assert (Euler002.sumOfEvenValuedTerms(100) === 44)
  }
  
  test("3rd problem: the largest prime factor of 13195 is 29: " ){
    assert (Euler003.largestPrimeFactor(13195) === 29)
  }
  
  test("4th problem: largest palindromic number that is a multiple of two two-digit numbers is 9009: " ){
    assert (Euler004.largestPalindrome(2) === 9009)
  }
  
  test("5th problem: the smallest number that can be divided by each of the numbers from 1 to ten is 2520: " ){
    assert (Euler005.lcmOfRange(1 to 10) === 2520)
  }
  
  test("6th problem: the difference between the sum of squares and the square of the sum of 1 to 10 is 2640: " ){
    assert (Euler006.squareOfSumMinusSumOfSquares(1 to 10) === 2640)
  }
  
  test("7th problem: the 6th prime number is 13: " ){
    assert (Euler007.indexedPrime(6) === 13)
  }
  
  test("8th problem: the greatest product of five consecutive numbers in 123451111 is 120: " ){
    assert (Euler008.findGreatestProduct("123451111", 5) === 120)
  }
  
  test("9th problem: the Pythagorean triple with sum 12 (3,4,5) has the product 60: " ){
    assert (Euler009.pythagoreanTripleProduct(12) === 60)
  }
  
  test("10th problem: the sum of all the primes below 10 is 17: " ){
    assert (Euler010.sumOfPrimesBelow(10) === 17)
  }
  
  test("11th problem: max product of four adjacent numbers is 70600674: " ){
    assert (Euler011.greatestProductOfAdjacentCells === 70600674)
  }

  test("12th problem: smallest triangle number with more than 5 divisors is 28: " ){
    assert (Euler012.triangleNumberWithMoreThanNDivisors(5) === 28)
  }

  test("13th problem: add a number block: " ){
    assert (Euler013.addNumbersAndGiveFirst10Digits("1000000000\n1") === "1000000001")
  }

  test("14th problem: the Collatz sequence starting at 13 has 10 terms"){
    assert (Euler014.collatzLength(13) === 10)
  }

  test("15th problem: there are six paths in a 2x2 grid"){
    assert (Euler015.calculateNumberOfPaths(2,2) === 6)
  }

  test("16th problem: sum of digits in 2^15 equals 26"){
    assert (Euler016.sumDigits(BigInt(2).pow(15)) === 26)
  }

  test("17th problem: numbers are translated into words correctly"){
    assert(Euler017.numberAsWord(342) === "three hundred and forty-two")
    assert(Euler017.numberAsWord(115) === "one hundred and fifteen")
    assert(Euler017.numberAsWord(1000) === "one thousand")
    assert(Euler017.numberOfCharacters(1 to 5) === 19)
    assert(Euler017.numberOfCharacters(1 to 1000) === 21124)
    assert(Euler017.numberOfCharacters(115 to 115) === 20)
    assert(Euler017.numberOfCharacters(342 to 342) === 23)
  }

  test("18th problem: find longest path in a triangular network"){
    assert(Euler018.getMaxPath(Array(Array(3),Array(7,4),Array(2,4,6),Array(8,5,9,3))) === 23)
  }

  test("20th problem: find the sum of digits of 100! Test with 10!"){
    assert(Euler020.factorial(BigInt(10)) === BigInt(3628800))
    assert(Euler020.sumOfDigits(Euler020.factorial(BigInt(10))) === 27)
  }

  test("21st problem: find amicable numbers"){
    assert(Euler021.isAmicable(28) === false)
    assert(Euler021.isAmicable(220) === true)
    assert(Euler021.isAmicable(1184) === true)
    assert(Euler021.isAmicable(5020) === true)
    assert(Euler021.isAmicable(6232) === true)
  }

  test("22nd problem: score names"){
    assert(Euler022.score(938, "COLIN") === 49714)
    assert(Euler022.sort(Euler022.getNames("names.txt"))(938-1) === "COLIN")
  }

  test("23rd problem: find numbers that cannot be written as sum of abundant numbers"){
    assert(Euler023.isAbundant(12) === true)
    assert(Euler023.isAbundant(11) === false)
  }

  test("28th problem: diagonals in the 5x5 spiral add up to 101"){
    assert(Euler028.sumOfCorners(5) === 101)
  }
}
