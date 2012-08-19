package org.draegisoft.euler

object Euler059 {
  def euler(): Unit ={
    val start = System.currentTimeMillis
    decrypt(io.Source.fromFile("cipher.txt").getLines.mkString, 3)
    val time = System.currentTimeMillis - start
    println("59th problem required %d ms.".format(time))
  }


  def decrypt(messageString: String, keyLength: Int): Unit = {
    val message = messageString.split(",").map(s => Integer.parseInt(s.trim)).toList
    val messageSplitInParts = (0 until keyLength).toList.map(i =>
      (0 until message.size).toList.filter(n => n%keyLength == i).map(c => message(c)))
    val messageSplitsUniqueElementsByFrequency = messageSplitInParts.map(list =>
      list.groupBy(x => x).mapValues(_.length).toList.sortBy{_._2}.reverse.map(x => x._1))

    val threeMostFrequent = List(' ', 'e', 't').map(_.toInt)
    val keyCandidatesPerSplit = messageSplitsUniqueElementsByFrequency.map(list => 
      threeMostFrequent.map(character => list.head^character).filter(character => 
        97 <= character && character <= 122))

    val keyCandidates = combinations(keyCandidatesPerSplit)
    for (key <- keyCandidates){
      val decryptedMessage = (0 until message.size).toArray.map(i =>
        message(i)^key(i % keyLength))
      val sum = decryptedMessage.sum
      val textMessage = decryptedMessage.map(_.toChar).mkString
      println("%d: %s".format(sum, textMessage))
    }
  }
}
