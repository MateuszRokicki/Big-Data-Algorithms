package Lab2

import Lab2.Ex2.{GetWords, shingles}

import java.io.{File, PrintWriter}
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Random
import Array._

object Ex3 {
  def main(args: Array[String]): Unit = {
    var books_path = List("D:\\Studia\\Magisterskie\\II semestr\\Big Data Algorithms\\Laboratorium\\Lab0_1\\files\\dracula1.txt",
      "D:\\Studia\\Magisterskie\\II semestr\\Big Data Algorithms\\Laboratorium\\Lab0_1\\files\\dracula2.txt",
      "D:\\Studia\\Magisterskie\\II semestr\\Big Data Algorithms\\Laboratorium\\Lab0_1\\files\\dracula3.txt",
      "D:\\Studia\\Magisterskie\\II semestr\\Big Data Algorithms\\Laboratorium\\Lab0_1\\files\\Mining of Massive Datasets\\chapter2.txt",
      "D:\\Studia\\Magisterskie\\II semestr\\Big Data Algorithms\\Laboratorium\\Lab0_1\\files\\Mining of Massive Datasets\\chapter3.txt",
      "D:\\Studia\\Magisterskie\\II semestr\\Big Data Algorithms\\Laboratorium\\Lab0_1\\files\\Mining of Massive Datasets\\chapter4.txt")
    var book_words = Map[String, Array[String]]()
    val pw = new PrintWriter(new File("output3.txt"))
    for(path <- books_path){
      var words = GetWords(path)
      var f = new File(path)
      var title = f.getName().split('.')(0)
      book_words += (title -> words)
    }
    /*
    var hash_function = Random.shuffle(List.range(0, all_shingles.size))
    var hash_table = Map[String, Int]()
    for(book <- char_matrix){
      for(h <- hash_function){
        if(book._2.contains(h)){
          hash_table += (book._1, h)
        }
      }
    }*/
    //println(char_matrix)

    //minshah functions matrix
    /*var hashfunctions = Array[Array[Int]]()
    for(i <- n){
      val a = range(0, i)
      hashfunctions  :+ range(0, i)
    }

    //hashfunctions = range(0,50)
    println(hashfunctions)
    for(i <- hashfunctions){
      for(j <- i){
        println(j)
      }
    }*/

    val n = List(10, 100, 250, 500)
    for(k <- 2 to 13){
      println("K-shingles size: " + k.toString)
      pw.write("K-shingles size: " + k.toString+"\n")
      var books_shingles = Map[String, Set[String]]()
      //pw.write(k.toString()+'\n')
      var jaccard = Map[String, Double]()
      for(book <- book_words){
        var kshingles = shingles(book._2, k)
        books_shingles += (book._1 -> kshingles)
      }
      /*for(path <- books_path){
        var kshingles = shingles(path, k)
        var f = new File(path)
        var title = f.getName().split('.')(0)
        books_shingles += (title -> kshingles)
      }*/

      //characteristic matrix
      var char_matrix = books_shingles.map(x=> (x._1, ListBuffer(0)))
      val all_shingles = books_shingles.values.toSet.flatten.toList
      for(i <- (0 to all_shingles.size-1)){
        for(book <- books_shingles){
          if(book._2.contains(all_shingles(i))){
            char_matrix(book._1) += i
          }
        }
      }
      for(i <- n){
        println("Hash functions size: " + i.toString)
        pw.write("Hash functions size: " + i.toString + "\n")
        var signatures = Signatures(char_matrix, i, all_shingles.size)
        var used_titles = List[String]()
        var jaccard = Map[String, Double]()
        var titles = books_shingles.keys.toList
        for(book1 <- books_shingles){
          used_titles :+= book1._1
          for(book2 <- books_shingles.filterKeys(!used_titles.contains(_)))
          {
            var jac = (book1._1 + ' ' + book2._1 -> JaccardSingatures(signatures(titles.indexOf(book1._1)), signatures(titles.indexOf(book2._1))))
            jaccard += jac
            pw.write(jac.toString() + '\n')
            println(jac)
          }
        }
      }
    }
  }

  def Signatures(characteristicMatrix : Map[String, ListBuffer[Int]], hashSize : Int, rows: Int) : Array[Array[Int]] = {
    //var a = Map[String, Map[Int, Int]]()//[tytul, [hashfunkcja, wartosc]]
    var titles = characteristicMatrix.keys.toList
    var signatures = Array.fill(characteristicMatrix.size, hashSize)(Int.MaxValue)
    //var signatures = Array.ofDim[Int](hashSize, characteristicMatrix.size)
    val hashFunctions =  Random.shuffle(range(0, hashSize))
    for(r <- range(0, rows)){
      for(hash <- hashFunctions){
        val h = (hash*r + 1)%rows
        for(book <- characteristicMatrix){
          if(book._2.contains(r)){
            if(signatures(titles.indexOf(book._1))(hash) > h) {
              signatures(titles.indexOf(book._1))(hash) = h
            }
          }
        }
      }
    }
    signatures
  }

  def JaccardSingatures(book1 : Array[Int], book2 : Array[Int]) : Double = {
    var sum = 0.0
    for(i <- range(0,book1.length)){
      if(book1(i) == book2(i)){
        sum+=1
      }
    }
    val jaccard = sum/(book1.length+book2.length)
    jaccard
  }

  /*def shingles(input_string : String, size : Int) : Set[String] = {
    var shingles = Set[String]()
    val stopwords = List("ourselves", "hers", "between", "yourself", "but", "again", "there", "about", "once", "during", "out", "very", "having", "with", "they", "own", "an", "be", "some", "for", "do", "its", "yours", "such", "into", "of", "most", "itself", "other", "off", "is", "s", "am", "or", "who", "as", "from", "him", "each", "the", "themselves", "until", "below", "are", "we", "these", "your", "his", "through", "don", "nor", "me", "were", "her", "more", "himself", "this", "down", "should", "our", "their", "while", "above", "both", "up", "to", "ours", "had", "she", "all", "no", "when", "at", "any", "before", "them", "same", "and", "been", "have", "in", "will", "on", "does", "yourselves", "then", "that", "because", "what", "over", "why", "so", "can", "did", "not", "now", "under", "he", "you", "herself", "has", "just", "where", "too", "only", "myself", "which", "those", "i", "after", "few", "whom", "t", "being", "if", "theirs", "my", "against", "a", "by", "doing", "it", "how", "further", "was", "here", "than")
    var lines = Source.fromFile(input_string).getLines().mkString(" ").toLowerCase()
    var words = lines.split("\\W+" ).filterNot(stopwords.contains(_))

    for(i <- 0 to (words.size - size - 1)){
      var shingle = ""
      for(j <- 0 to size-1){
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
  }*/
}
