package org.draegisoft.euler

import collection.immutable.Seq

object Euler extends App{
    println ("Solution to 1st problem: " + Euler1.euler(Seq(3,5), 1000))
    println ("Solution to 2nd problem: " + Euler2.euler(4000000))
    println ("Solution to 3rd problem: " + Euler3.euler(600851475143L))
    println ("Solution to 4th problem: " + Euler4.euler(3))
    println ("Solution to 5th problem: " + Euler5.euler(1, 20))
    println ("Solution to 6th problem: " + Euler6.euler(1, 100))
    println ("Solution to 7th problem: " + Euler7.euler(10001))
    println ("Solution to 8th problem: " + Euler8.euler(Euler8.numString, 5))
    println ("Solution to 9th problem: " + Euler9.euler(1000))
}
