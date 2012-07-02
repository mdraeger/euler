package org.draegisoft.euler

object Euler067 {
  def euler(): Unit ={
    val start = System.currentTimeMillis
    val maxPath = Euler018.getMaxPath(euler067network)
    val time = System.currentTimeMillis - start
    println("67th problem: %d required %d ms.".format(maxPath, time))
  }

  val euler067network = {
    val lines = scala.io.Source.fromFile("triangle.txt").getLines.toArray
    lines.map (line => line.split(" ").filter(s => s != "").map(n => Integer.parseInt(n)))
  }
}
