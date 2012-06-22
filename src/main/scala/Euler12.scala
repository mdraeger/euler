package org.draegisoft.euler

object Euler12 {
  val primesToConsider = primes take (100000) toList
  def euler(upperNumberOfDivisors: Int): Long = {
    var n = 1
    var triangle = 0L
    while (numberOfDivisors(triangle) <= upperNumberOfDivisors){
        triangle += n
        n += 1
    }
    triangle
  }

  def numberOfDivisors(number: Long): Int = {
    if (isPrime(number))
      2
    else
      (1 /: (factorMultiplicities(primesToConsider, number) map (_ + 1)))(_ * _)
  }

  def factorMultiplicities(primeList: List[Long], number: Long): List[Int] = {
    val currentPrime = primeList.head
    if (number <= 1)
      Nil
    else if (number % currentPrime != 0)
      factorMultiplicities(primeList.tail, number)
    else {
      val mult = multiplicity(currentPrime, number)
      val remainder = number / (math.pow(currentPrime, mult).toInt)
      mult :: factorMultiplicities(primeList.tail, remainder)
    }
  } 

  def multiplicity(prime: Long, number: Long): Int = {
    var m = 0
    var pNum = number
    while (pNum % prime == 0){
      m += 1
      pNum /= prime
    }
    m
  }

  def isPrime(candidate: Long) = primesToConsider contains (candidate)
}
