package CSSR.parse

import scala.collection.immutable.Map
import scala.io.Source

object Parse {
  def apply(a:String, b:String) = new Parse(a, b)

}

class Parse(alphabetFile:String, dataSource:String) {
  val alphabet:Map[Char, Int] = loadAlphabet(alphabetFile)
  val tree = loadData(dataSource)

  def loadAlphabet(filePath: String): Map[Char, Int] = {
    val source = Source.fromFile(filePath)
    try {
      source.mkString
        .zipWithIndex
        .foldLeft(Map[Char, Int]()) ((acc, next) => {
          val (char, index) = next
          acc + (char -> index)
        })
    } finally source.close()
  }

  def loadData (filePath: String): ParseTree = {
    val source = Source.fromFile(filePath)
    try {
      for (line <- source.getLines){

      }
    } finally source.close()
  }
}
