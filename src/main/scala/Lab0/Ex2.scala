package Lab0

import scala.collection.mutable.{ListBuffer, Map}
import scala.io.Source

object Ex2 {
  def main(args: Array[String]): Unit = {
    val stopwords = List("ourselves", "hers", "between", "yourself", "but", "again", "there", "about", "once", "during", "out", "very", "having", "with", "they", "own", "an", "be", "some", "for", "do", "its", "yours", "such", "into", "of", "most", "itself", "other", "off", "is", "s", "am", "or", "who", "as", "from", "him", "each", "the", "themselves", "until", "below", "are", "we", "these", "your", "his", "through", "don", "nor", "me", "were", "her", "more", "himself", "this", "down", "should", "our", "their", "while", "above", "both", "up", "to", "ours", "had", "she", "all", "no", "when", "at", "any", "before", "them", "same", "and", "been", "have", "in", "will", "on", "does", "yourselves", "then", "that", "because", "what", "over", "why", "so", "can", "did", "not", "now", "under", "he", "you", "herself", "has", "just", "where", "too", "only", "myself", "which", "those", "i", "after", "few", "whom", "t", "being", "if", "theirs", "my", "against", "a", "by", "doing", "it", "how", "further", "was", "here", "than")
    var lines = Source.fromFile("D:\\Studia\\Magisterskie\\II semestr\\Big Data Algorithms\\Laboratorium\\Lab0_1\\files\\Lab_0\\orwell1984.txt").getLines().mkString(" ")
    var words_f = lines.split(" ")
    var words = ListBuffer[String]()
    for (word <- words_f) {
      var word_L = word.toLowerCase().replaceAll("\\p{Punct}", "")
      if (!stopwords.contains(word_L)) {
        words += word_L
      }
    }
    val word_count: Map[String, Int] = Map()
    for (word <- words) {
      if (word_count.contains(word)) {
        word_count(word) += 1
      }
      else {
        word_count += (word -> 1)
      }
    }
    val res = word_count.toSeq.sortWith(_._2 > _._2)
    for (r <- res.take(41)) {
      if(r._1!=""){
        println(r._2 + ", " + r._1)
      }
    }
  }
}
