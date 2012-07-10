package org.draegisoft

package object euler {
  lazy val primes: Stream[Long] = Stream.cons(2, naturals(3,2) filter{
      n => primes.takeWhile(p=> p*p <= n).forall(n % _ != 0)
    })

  def naturals(from: Long, step: Long): Stream[Long] = {
    lazy val result: Stream[Long] = Stream.cons(from, result map (_ + step))
    result
  }

  def lcm(a: Long, b: Long): Long = math.abs(a * b).toLong / gcd(a, b)

  def gcd(a: Long, b: Long): Long = {
    if (b == 0)
      a
    else 
      gcd(b, a % b)
  }
  
  def sumOfProperDivisors(n: Int) = {
    var sum = 1
    val root = math.sqrt(n).toInt
    for(i <- 2 to root + 1)
      if (n % i == 0){
      sum += i + n/i
    }
    if (n == root * root)
      sum -= root

    sum
  }

  def prettyPrint(array: Array[Array[Int]]): Unit = {
    for (i <- 0 until array.length){
      for (j <- 0 until array(i).length)
        print(array(i)(j) + " ")
      println()
    }
  }
}

