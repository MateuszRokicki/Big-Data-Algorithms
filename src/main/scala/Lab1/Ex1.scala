package Lab1

import scala.collection.mutable.{ListBuffer, Map}
import scala.io.Source
import java.io.{File, FileNotFoundException, PrintWriter}


class Text() {

  var title  = ""
  var word_count: Map[String, Int] = Map()
  var sorted_words = Map[String,Int]().toSeq
  val stopwords = List("ourselves", "hers", "between", "yourself", "but", "again", "there", "about", "once", "during", "out", "very", "having", "with", "they", "own", "an", "be", "some", "for", "do", "its", "yours", "such", "into", "of", "most", "itself", "other", "off", "is", "s", "am", "or", "who", "as", "from", "him", "each", "the", "themselves", "until", "below", "are", "we", "these", "your", "his", "through", "don", "nor", "me", "were", "her", "more", "himself", "this", "down", "should", "our", "their", "while", "above", "both", "up", "to", "ours", "had", "she", "all", "no", "when", "at", "any", "before", "them", "same", "and", "been", "have", "in", "will", "on", "does", "yourselves", "then", "that", "because", "what", "over", "why", "so", "can", "did", "not", "now", "under", "he", "you", "herself", "has", "just", "where", "too", "only", "myself", "which", "those", "i", "after", "few", "whom", "t", "being", "if", "theirs", "my", "against", "a", "by", "doing", "it", "how", "further", "was", "here", "than")
  var sumwords = 0
  var mostfrequentwords = Map[String,Int]().toSeq
  def CountWords(input_string:String): Unit = {
    var lines = Source.fromFile(input_string).getLines().mkString(" ").toLowerCase()
    var words_f = lines.split("\\W+" ).filterNot(stopwords.contains(_))
    val word_count2 = words_f.groupBy(identity).map(x=> (x._1, x._2.size))
    for(word <- word_count2){
      if(word != ""){
        word_count.getOrElseUpdate(word._1,0)
        word_count(word._1) += word._2
      }
    }
    //sorted_words = word_count.toSeq.sortWith(_._2 > _._2)
  }

  def SortWords() : Unit = {
    sorted_words = word_count.toSeq.sortWith(_._2 > _._2)
  }
  def Word_Cloud(n:Int): Unit = {
    mostfrequentwords = sorted_words.take(n)
    println(title, mostfrequentwords)
  }

  def CountAllWords() : Unit = {
    sumwords = word_count.foldLeft(0)(_+_._2)
  }


}

object Ex1 {
  def main(args: Array[String]): Unit = {
    val n = 10
    //load books
    var books = List[Text]()
    var all_books_tfidf = Map[String, Map[String, Double]]()
    var all_words = Map[String, Int]()
    var books_path = List("D:\\Studia\\Magisterskie\\II semestr\\Big Data Algorithms\\Laboratorium\\Lab0_1\\files\\alicesadventuresinwonderland.txt",
      "D:\\Studia\\Magisterskie\\II semestr\\Big Data Algorithms\\Laboratorium\\Lab0_1\\files\\dracula.txt",
      "D:\\Studia\\Magisterskie\\II semestr\\Big Data Algorithms\\Laboratorium\\Lab0_1\\files\\frankenstein.txt",
      "D:\\Studia\\Magisterskie\\II semestr\\Big Data Algorithms\\Laboratorium\\Lab0_1\\files\\littlewomen.txt",
      "D:\\Studia\\Magisterskie\\II semestr\\Big Data Algorithms\\Laboratorium\\Lab0_1\\files\\mobydick.txt",
      "D:\\Studia\\Magisterskie\\II semestr\\Big Data Algorithms\\Laboratorium\\Lab0_1\\files\\orwell1984.txt",
      "D:\\Studia\\Magisterskie\\II semestr\\Big Data Algorithms\\Laboratorium\\Lab0_1\\files\\peterpan.txt",
      "D:\\Studia\\Magisterskie\\II semestr\\Big Data Algorithms\\Laboratorium\\Lab0_1\\files\\theadventuresofsherlockholmes.txt",
      "D:\\Studia\\Magisterskie\\II semestr\\Big Data Algorithms\\Laboratorium\\Lab0_1\\files\\thegreatgatsby.txt",
      "D:\\Studia\\Magisterskie\\II semestr\\Big Data Algorithms\\Laboratorium\\Lab0_1\\files\\theodyssey.txt",
      "D:\\Studia\\Magisterskie\\II semestr\\Big Data Algorithms\\Laboratorium\\Lab0_1\\files\\thescarletletter.txt")

    var books_path2 = List("D:\\Studia\\Magisterskie\\II semestr\\Big Data Algorithms\\Laboratorium\\Lab0_1\\files\\Mining of Massive Datasets\\chapter2.txt",
      "D:\\Studia\\Magisterskie\\II semestr\\Big Data Algorithms\\Laboratorium\\Lab0_1\\files\\Mining of Massive Datasets\\chapter3.txt",
      "D:\\Studia\\Magisterskie\\II semestr\\Big Data Algorithms\\Laboratorium\\Lab0_1\\files\\Mining of Massive Datasets\\chapter4.txt",
      "D:\\Studia\\Magisterskie\\II semestr\\Big Data Algorithms\\Laboratorium\\Lab0_1\\files\\Mining of Massive Datasets\\chapter5.txt",
      "D:\\Studia\\Magisterskie\\II semestr\\Big Data Algorithms\\Laboratorium\\Lab0_1\\files\\Mining of Massive Datasets\\chapter6.txt",
      "D:\\Studia\\Magisterskie\\II semestr\\Big Data Algorithms\\Laboratorium\\Lab0_1\\files\\Mining of Massive Datasets\\chapter7.txt",
      "D:\\Studia\\Magisterskie\\II semestr\\Big Data Algorithms\\Laboratorium\\Lab0_1\\files\\Mining of Massive Datasets\\chapter8.txt",
      "D:\\Studia\\Magisterskie\\II semestr\\Big Data Algorithms\\Laboratorium\\Lab0_1\\files\\Mining of Massive Datasets\\chapter9.txt",
      "D:\\Studia\\Magisterskie\\II semestr\\Big Data Algorithms\\Laboratorium\\Lab0_1\\files\\Mining of Massive Datasets\\chapter10.txt",
      "D:\\Studia\\Magisterskie\\II semestr\\Big Data Algorithms\\Laboratorium\\Lab0_1\\files\\Mining of Massive Datasets\\chapter11.txt")

    for(path <- books_path2) {
      //Create new object Text for each book(path)
      var f = new File(path)
      var book = new Text()
      book.CountWords(path)
      book.SortWords()
      book.title = f.getName().split('.')(0)
      book.CountAllWords()
      //Add every book to list of books
      books :+= book

      //Add words from book to Map of words from all books
      for (word <- book.word_count) {
        if (all_words.contains(word._1)) {
          all_words(word._1) += word._2
        }
        else {
          all_words += (word._1 -> word._2)
        }
      }
    }

    //print n most frequent elements from every book
    for(book <- books){
      book.Word_Cloud(n)
    }

    //print n most frequent elements from all books
    var all_books = new Text()
    all_books.word_count = all_words
    all_books.SortWords()
    all_books.Word_Cloud(n)


    //calculate TF.IDF for n words
    for(book <- books){
      var book_tfidf = Map[String, Double]()
      for(word <- book.word_count){
        var tf = word._2.toFloat/book.sumwords.toFloat
        var cnt = 0
        for(book <- books){
          if(book.word_count.contains(word._1)){
            cnt += 1
          }
        }
        var idf = Math.log(books.size.toFloat/cnt.toFloat)
        book_tfidf(word._1) = tf*idf
      }
      all_books_tfidf(book.title) = book_tfidf
    }
    for(book <-all_books_tfidf){
      println(book._1)
      var sortedwords = book._2.toSeq.sortWith(_._2 > _._2)
      for(words <- sortedwords.take(n)){
        println(words)
      }
    }
  }
}
