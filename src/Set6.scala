import scala.collection.immutable.ListMap
import java.io.{FileNotFoundException, IOException}

object ExerciseSet6 extends App {
  val myName: String = "Lauri Järvisalo"
  val myStudentid: String = "xxxxxxx"
  val solvedExercises: Array[Int] = Array(1) // for example = Array(1,2,5,6)

  // Exercise 1. (8p)
  // Create a frequency dictionary of the words in a set of files. A frequence dictionary has
  // word as key and the number of occurencies of the word in the input material as value.
  // Find from the frequency dictionary the 15 most common words.
  // Write both a sequencial solution and a parallel solution, using futures. Compare their
  // execution times (you can use java.lang.System.currentTimeMillis(), for example).

  // Material for the exercise can be found at "https://users.metropolia.fi/~peterh/parts/part[1-7].txt"
  // (7 files from http://www.fullbooks.com/The-Sea-Wolf.html). If you want to experiment with larger set of
  // files, the directory has part100.txt - part999.txt (they are similar to each other). You can also experiment
  // reading the files from your local disk - that might be useful when developing the application. Copy the files
  // with curl or similar in that case.

  // Try to write your code in functional style. Write first small utility functions, for example, for splitting
  // a string into words etc. and compose the whole application from those. If you feel you need program in
  // imperative style, using mutable data etc., try to encapsulate those parts inside functions.

  // Hints: check Horstmann book chapter 9 for how to read the data and do some of the processing.


  /////////////////////////
  // Sequential solution //
  /////////////////////////

  def getPart(a: Int): String = {
    val fileName = "https://users.metropolia.fi/~peterh/parts/part"+a+".txt"
    fileName
  }

  def textToString(fileName: String): String = {
    var rawText: String = ""
    try {
      for(line <- scala.io.Source.fromURL(fileName).getLines()) {
        rawText += line + " "
      }
    } catch {
      case e: FileNotFoundException => println("File not found!")
      case e: IOException => println("IOException!")
    }
    rawText
  }

  def cleanString(text: String): String = {
    val dirtyText = text.replaceAll("[\\-\\,\\.\\:\\;\\?\\!\\"+'"'+"]", "")
    val cleanText = dirtyText.toLowerCase.capitalize
    cleanText
  }

  def stringToList(text: String): List[String] = {
    val wordList: List[String] = text.split(" ").map(_.trim).toList
    wordList.filter(_.nonEmpty)
  }

  def listToMap(wordList: List[String]): Map[String, Int] = {
    val counts: Map[String, Int] = wordList.foldLeft(Map.empty[String, Int]) { (map, string) =>
      val count: Int = map.getOrElse(string, 0)
      map.updated(string, count + 1)
    }
    ListMap(counts.toSeq.sortWith(_._2 > _._2):_*)
  }

  def getMostFrequentWords(wordMap: Map[String, Int]): String = {
    var x = 0
    var result = "The 15 most frequent words are:"
    for ((k, v) <- wordMap) {
      if (x < 15) {
        result += "\nWord = "+k+"  -  "+v+" instances"
        x += 1
      } else result
    }
    result
  }

  def getAllParts(): List[String] = {
    var listOfAll = List[String]()
    var counter = 1
    while(counter <= 7) {
      val fileName = getPart(counter)
      val rawText = textToString(fileName)
      val cleanText = cleanString(rawText)
      val stringList = stringToList(cleanText)
      listOfAll = listOfAll ++ stringList
      counter += 1
    }
    listOfAll
  }

  // Main function for sequential solution
  def sequentialSolution(): String = {
    val timer = System.nanoTime
    val list = getAllParts()
    //println(list)
    val map = listToMap(list)
    val result = getMostFrequentWords(map)
    val duration = (System.nanoTime - timer) / 1e9d
    println(result)
    println("\nDuration = "+duration+" seconds")
    result
  }

  // Look for a single word (basically a function for error checking)
  def checkWord(list: List[String], lookFor: String): Int = {
    var counter = 0
    for(word <- list) {
      if(word == lookFor) {
        counter += 1
      }
    }
    println("The word "+'"'+lookFor+'"'+" found "+counter+" times")
    counter
  }
}

object MainTest extends App {

  import ExerciseSet6._

  sequentialSolution()

}