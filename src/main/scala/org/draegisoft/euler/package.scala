package org.draegisoft

package object euler {
  lazy val primes: Stream[Long] = Stream.cons(2, naturals(3,2) filter{
      n => primes.takeWhile(p=> p*p <= n).forall(n % _ != 0)
    })

  def naturals(from: Long, step: Long): Stream[Long] = {
    lazy val result: Stream[Long] = Stream.cons(from, result map (_ + step))
    result
  }
}

