package org.draegisoft.euler

import collection.immutable.Seq

object Euler extends App{
    println ("Solution to 1st problem: " + Euler1.euler1(Seq(3,5), 1000))
    println ("Solution to 2nd problem: " + Euler2.euler2(4000000))
    println ("Solution to 3rd problem: " + Euler3.euler3(600851475143L))
    println ("Solution to 4th problem: " + Euler4.euler4(3))
}
