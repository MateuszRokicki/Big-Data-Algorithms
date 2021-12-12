package Lab2

//import scala.collection.mutable.Map
import java.io.File
import scala.io.Source

object Ex2 {
  def main(args: Array[String]): Unit = {
    //load books
    var books_shingles = Map[String, Set[String]]()
    var k_jaccard = Map[Int, Map[String,Double]]()
    var books_path = List("D:\\Studia\\Magisterskie\\II semestr\\Big Data Algorithms\\Laboratorium\\Lab0_1\\files\\dracula.txt",
      "D:\\Studia\\Magisterskie\\II semestr\\Big Data Algorithms\\Laboratorium\\Lab0_1\\files\\frankenstein.txt",
      "D:\\Studia\\Magisterskie\\II semestr\\Big Data Algorithms\\Laboratorium\\Lab0_1\\files\\mobydick.txt",
      "D:\\Studia\\Magisterskie\\II semestr\\Big Data Algorithms\\Laboratorium\\Lab0_1\\files\\Mining of Massive Datasets\\chapter2.txt",
      "D:\\Studia\\Magisterskie\\II semestr\\Big Data Algorithms\\Laboratorium\\Lab0_1\\files\\Mining of Massive Datasets\\chapter3.txt",
      "D:\\Studia\\Magisterskie\\II semestr\\Big Data Algorithms\\Laboratorium\\Lab0_1\\files\\Mining of Massive Datasets\\chapter4.txt")

    //create set of k-shingles
    for(k <- 2 to 13){
      println(k)
      var jaccard = Map[String, Double]()
      for(path <- books_path){
        var kshingles = shingles(path, k)
        var f = new File(path)
        var title = f.getName().split('.')(0)
        books_shingles += (title -> kshingles)
      }
      var used_titles = List[String]()
      for(book1 <- books_shingles){
        used_titles :+= book1._1
        for(book2 <- books_shingles.filterKeys(!used_titles.contains(_)))
        {
          var jac = (book1._1 + ' ' + book2._1 -> Jaccard_Similarity(book1._2, book2._2))
          jaccard += jac
          println(jac)
        }
      }
      k_jaccard += (k -> jaccard)
    }
  }


  def Jaccard_Similarity(book1 : Set[String], book2 : Set[String]) : Double = {
    var jaccard = (book1 intersect book2).size.toDouble/(book1 union book2).size.toDouble
    return jaccard
  }

  def shingles(input_string : String, size : Int) : Set[String] = {
    var shingles = Set[String]()
    val stopwords = List("ourselves", "hers", "between", "yourself", "but", "again", "there", "about", "once", "during", "out", "very", "having", "with", "they", "own", "an", "be", "some", "for", "do", "its", "yours", "such", "into", "of", "most", "itself", "other", "off", "is", "s", "am", "or", "who", "as", "from", "him", "each", "the", "themselves", "until", "below", "are", "we", "these", "your", "his", "through", "don", "nor", "me", "were", "her", "more", "himself", "this", "down", "should", "our", "their", "while", "above", "both", "up", "to", "ours", "had", "she", "all", "no", "when", "at", "any", "before", "them", "same", "and", "been", "have", "in", "will", "on", "does", "yourselves", "then", "that", "because", "what", "over", "why", "so", "can", "did", "not", "now", "under", "he", "you", "herself", "has", "just", "where", "too", "only", "myself", "which", "those", "i", "after", "few", "whom", "t", "being", "if", "theirs", "my", "against", "a", "by", "doing", "it", "how", "further", "was", "here", "than")
    var lines = Source.fromFile(input_string).getLines().mkString(" ").toLowerCase()
    var words = lines.split("\\W+" ).filterNot(stopwords.contains(_))

    for(i <- 0 to (words.size - size - 1)){
      var shingle = ""
      for( j <- 0 to size-1){
        if(j < size-1){
          shingle += words(i+j) + ' '
        }
        else{
          shingle += words(i+j)
        }
      }
      shingles += shingle
    }
    shingles
  }
}