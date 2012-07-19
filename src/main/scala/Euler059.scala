package org.draegisoft.euler

object Euler059 {
  def euler(): Unit ={
    val start = System.currentTimeMillis
    val sum = sumOfBytes("cipher.txt", 3)
    val time = System.currentTimeMillis - start
    println("59th problem: %d required %d ms.".format(sum, time))
  }

  def sumOfBytes(filename: String, keyLength: Int): Int = {
    0
  }

  def decrypt(filename: String, keyLength: Int): Array[Byte] = {
    val charactersByFrequency = Array('e','t','a','o','i','n','s','h','r','d','l','c','u','m','w','f','g','y','p','b','v','k','j','x','q','z')
    Array(0)
  }

  def cipherArrays(filename: String, keyLength: Int) = {
    val original = cipherArray(filename)
    (0 until keyLength).map(i =>
      (0 until original.length).filter(n => n % keyLength == i).map(c => original(c)))
    }
    
  def cipherArray(filename: String) = io.Source.fromFile(filename).getLines.mkString.split(",").map(s => Integer.parseInt(s))
}
