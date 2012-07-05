package org.draegisoft.euler

object Euler022 {
  def euler(): Unit ={
    val start = System.currentTimeMillis
    val sum = sumOfScores(getNames("names.txt"))
    val time = System.currentTimeMillis - start
    println("22th problem: %d required %d ms.".format(sum, time))
  }

  def sumOfScores(names: Array[String]): Long = {
    val sortedNames = sort(names)
    var sum = 0
    for (i <- 0 until sortedNames.length)
      sum += score(i+1, sortedNames(i))
    sum
  }

  def sort(names: Array[String]): Array[String] = {
    if (names.length <= 1)
      names
    else{
      val pivot = names(0)
      sort(names.filter(name => name < pivot)) ++ Array(pivot) ++ sort(names.filter(name => name > pivot))
    }
  }

  def score(index: Int, name: String): Int = {
    val sum = (0 /: name.map(c => c-64)) (_ + _)
    index * sum
  }
 
  def getNames(fileName: String): Array[String] = {
    io.Source.fromFile(fileName).getLines.mkString.split(",").map(s => s.replaceAll("\"", ""))
  }
}
