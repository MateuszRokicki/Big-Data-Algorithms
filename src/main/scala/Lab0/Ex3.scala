package Lab0

import scala.collection.mutable.{ListBuffer, Map}
import scala.io.Source
import java.io.{File, FileNotFoundException, PrintWriter}



class Text() {

  private var word_count: Map[String, Int] = Map()
  var sorted_words = Map[String,Int]().toSeq
  val stopwords = List("ourselves", "hers", "between", "yourself", "but", "again", "there", "about", "once", "during", "out", "very", "having", "with", "they", "own", "an", "be", "some", "for", "do", "its", "yours", "such", "into", "of", "most", "itself", "other", "off", "is", "s", "am", "or", "who", "as", "from", "him", "each", "the", "themselves", "until", "below", "are", "we", "these", "your", "his", "through", "don", "nor", "me", "were", "her", "more", "himself", "this", "down", "should", "our", "their", "while", "above", "both", "up", "to", "ours", "had", "she", "all", "no", "when", "at", "any", "before", "them", "same", "and", "been", "have", "in", "will", "on", "does", "yourselves", "then", "that", "because", "what", "over", "why", "so", "can", "did", "not", "now", "under", "he", "you", "herself", "has", "just", "where", "too", "only", "myself", "which", "those", "i", "after", "few", "whom", "t", "being", "if", "theirs", "my", "against", "a", "by", "doing", "it", "how", "further", "was", "here", "than")

  def CountWords(input_string:String): Unit = {
    var lines = Source.fromFile(input_string).getLines().mkString(" ")
    var words_f = lines.split(" ")
    var words = ListBuffer[String]()
    for (word <- words_f) {
      var word_L = word.toLowerCase().replaceAll("\\p{Punct}", "")
      if (!stopwords.contains(word_L)) {
        words += word_L
      }
    }
    for (word <- words) {
      if (word_count.contains(word)) {
        word_count(word) += 1
      }
      else {
        word_count += (word -> 1)
      }
    }
  }

  def Word_Cloud(): Unit = {
    sorted_words = word_count.toSeq.sortWith(_._2 > _._2)
    for (s <- sorted_words.take(40)) {
      if(s._1!=""){
        println(s._2 + " " + s._1)
      }
    }
  }

  def Save(path:String, title: String): Unit = {
    val file_path = path + '\\' + title + ".csv"
    val pw = new PrintWriter(new File(file_path))
    for(s<-sorted_words.take(41)){
      if(s._1!=""){
        pw.write(s._2 + ", " + s._1 + '\n')
      }
    }
    pw.close()
  }
}

object Ex3 {
  def main(args: Array[String]): Unit = {
    var exit: Boolean = true
    var words = new Text()
    while(exit){
      try{
        val input = io.StdIn.readLine("Enter file location: ")
        if(input == "exit"){
          val file_path = io.StdIn.readLine("Enter file path: ")
          val file_title = io.StdIn.readLine("Enter file name: ")
          words.Save(file_path, file_title)
          exit = false
        }
        else{
          words.CountWords(input)
          words.Word_Cloud()
        }
      }
      catch{
        case e: FileNotFoundException => println("Wrong path")
      }
    }
  }
}
