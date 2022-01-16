package Lab3
import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}
import org.jsoup.select.Elements

import java.io.{File, PrintWriter}
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.util.control.Breaks.break



object Ex1 {
  def main(args: Array[String]): Unit = {
    //var a = CrawlMode1(100)
    //println(a)
    var (matrix, links) = CrawlMode2(1000)
    //println(links)
    println("1")
    PageRank(50, matrix, links)
    //LinkAnalysisMode("https://en.wikipedia.org/wiki/Dan_Aykroyd", links, matrix)
  }


  def LinkAnalysisMode(link : String, allLinks:List[(Int, String)], matrix: Array[Array[Double]]) : Unit={
    val id = allLinks.find(_._2 == link).get._1
    var contain_link = ListBuffer[(Int, String)]()
    for(i <- 0 to matrix.length-1){
      if(matrix(id)(i) != 0.0){
        contain_link += allLinks.find(_._1 == i).get
      }
    }
    println("Pages which have links to " + link +":")
    contain_link.foreach(x=> println(x))
    println("Number of pages: " + contain_link.size)
    var cnt = 0
    matrix.foreach(x=> x.foreach(y=> if(y != 0.0){cnt+=1}))
    println("Average number of links per page: " + cnt.toDouble/matrix.length.toDouble)
  }


  def PageRank(n : Int, matrix : Array[Array[Double]], links : List[(Int, String)]) {
    val base_matrix = matrix
    var change_matrix = matrix
    var prob  = 1/matrix.length.toDouble
    var cnt = 1
    val t1 = System.nanoTime()
    while(cnt < n){
      var prod = Array.ofDim[Double](matrix.length,matrix.length)
      var i = 0
      while(i < matrix.length){
        var j = 0
        while(j < matrix.length){
          prod(i)(j) = 0
          var k = 0
          while(k < matrix.length){
            val a = base_matrix(i)(k)
            val b = change_matrix(k)(j)
            prod(i)(j) += a * b
            k += 1
          }
          j += 1
        }
        i += 1
      }
      change_matrix = prod
      cnt += 1
    }

    /*for(i <- 0 to matrix.length-1){
      for(j <- 0 to matrix.length-1){
        print(" " +change_matrix(i)(j))
      }
      println()
    }*/
    println((System.nanoTime()-t1)/1e9d)
    var i = 0
    var result = new Array[Double](matrix.length)
    while(i < matrix.length){
      var j = 0
      while(j < matrix.length){
        var v = change_matrix(i)(j)
        result(i) += v * prob
        j += 1
      }
      //println(result(i))
      i+=1
    }
    val pw = new PrintWriter(new File("pageRank.txt"))
    for(el <- links){
      pw.write(el._2 +" " + result(el._1).toString + "\n")
    }
    pw.close()
    println((System.nanoTime()-t1)/1e9d)
  }

  def CrawlMode1(n : Int) : List[(Int, Int, String, ListBuffer[Int], ListBuffer[Int])]={
    val doc = Jsoup.connect("https://en.wikipedia.org/").get()
    var firstLinks : Elements  = doc.select("a[href^=/wiki/]")
    var links = firstLinks.select("a:not([class])")
    println(links.size)
    //ID, flag, link, out, int
    var allLinks = List[(Int, Int, String, ListBuffer[Int], ListBuffer[Int])]()
    //allLinks :+= (0, 1, "", ListBuffer[Int](), ListBuffer[Int]())
    var i = 0
    for(el <- links.asScala){
      if(!el.attr("href").contains(":") && !allLinks.exists(_._3 == el.attr("href"))){
        if(i < n){
          allLinks :+= (i, 1, el.attr("href"), ListBuffer[Int](), ListBuffer[Int]())
        }
        else{
          allLinks :+= (i, 0, el.attr("href"), ListBuffer[Int](), ListBuffer[Int]())
        }
        i += 1
      }
    }
    if(i >= n){
      for(link <- allLinks){
        val doc = Jsoup.connect("https://en.wikipedia.org" + link._3).get()
        val firstLinks : Elements  = doc.select(".vector-body").select("a[href^=/wiki/]")
        val links = firstLinks.select("a:not([class])")
        for(el <- links.asScala){
          val newLink = el.attr("href")
          if(!newLink.contains(":")){
            if(!allLinks.exists(_._3 == newLink)){
              if(i < n){
                allLinks :+= (i, 1, newLink, ListBuffer[Int](), ListBuffer(link._1))
              }
              else{
                allLinks :+= (i, 0, newLink, ListBuffer[Int](), ListBuffer(link._1))
              }
            }
            else{
              allLinks.find(_._3==newLink).get._5 += i
            }
            i +=1
            link._4 += i
          }
        }
        println(allLinks.size)
      }
    }
    else{
      while(i < n){
        for(link <- allLinks if link._2 == 1){
          val doc = Jsoup.connect("https://en.wikipedia.org" + link._3).get()
          val firstLinks : Elements  = doc.select("a[href^=/wiki/]")
          val links = firstLinks.select("a:not([class])")
          for(el <- links.asScala){
            val newLink = el.attr("href")
            if(!el.attr("href").contains(":")){
              i +=1
              if(!allLinks.exists(_._3 == newLink)){
                if(i < n){
                  allLinks :+= (i, 1, newLink, ListBuffer[Int](), ListBuffer(link._1))
                }
                else{
                  allLinks :+= (i, 0, newLink, ListBuffer[Int](), ListBuffer(link._1))
                }
              }
              else{
                allLinks.find(_._3==newLink).get._5 += i
              }
              link._4 += i
            }
          }
          println(allLinks.size)
        }
      }
    }
    println(allLinks.size)
    //after loop while
    //loop over alllinks, adjust int and out lists
    /*for(el <- mainPageLinks){
      var partLink = el._2.attr("href")
      val doc = Jsoup.connect("https://en.wikipedia.org/" + partLink).get()
    }*/
    allLinks.foreach(x=> {if(x._2 == 1){println(x)}})

    allLinks.filter(_._2 == 1)
  }

  def CrawlMode2(n : Int) : (Array[Array[Double]], List[(Int, String)]) = {
    val doc = Jsoup.connect("https://en.wikipedia.org").get()
    var firstLinks: Elements = doc.select("a[href^=/wiki/]")
    var links = firstLinks.select("a:not([class])")
    //ID, link, out
    var allLinks = List[(Int, String, ListBuffer[Int])]()
    allLinks :+= (0, "https://en.wikipedia.org", ListBuffer[Int]())
    var i = 1
    for (el <- links.asScala) {
      if (!el.attr("href").contains(":") && !allLinks.exists(_._3 == el.attr("href"))) {
        if (i < n) {
          allLinks :+= (i, "https://en.wikipedia.org" + el.attr("href"), ListBuffer[Int](0))
          i += 1
        }
      }
    }
    if(i < n){
      while(i < n){
        for(link <- allLinks){
          val doc = Jsoup.connect(link._2).get()
          val firstLinks : Elements  = doc.select("a[href^=/wiki/]")
          val links = firstLinks.select("a:not([class])")
          for(el <- links.asScala){
            val newLink = "https://en.wikipedia.org" + el.attr("href")
            if(!el.attr("href").contains(":")){
              if(!allLinks.exists(_._2 == newLink)){
                if(i < n){
                  allLinks :+= (i, newLink, ListBuffer[Int]())
                  i += 1
                }
              }
            }
          }
        }
      }
    }
   for(link <- allLinks) {
     val doc = Jsoup.connect(link._2).get()
     val firstLinks : Elements  = doc.select(".vector-body").select("a[href^=/wiki/]")
     val links = firstLinks.select("a:not([class])")
     for(el <- links.asScala){
       val newLink = "https://en.wikipedia.org" + el.attr("href")
       if(allLinks.exists(_._2 == newLink)){
           if(!link._3.contains(allLinks.find(_._2 == newLink).get._1)){
             link._3 += allLinks.find(_._2 == newLink).get._1
           }
            /*if(!allLinks.find(_._2 == newLink).get._3.contains(link._1)){
              allLinks.find(_._2 == newLink).get._3 += link._1
            }*/
         }
       }
    }
    var mat = Array.ofDim[Double](n,n)
    for(i <- 0 to n-1){
      for(j <- 0 to n-1){
        mat(i)(j) = 0
      }
    }
    for(link <- allLinks) {
      var p : Double = 1.0/link._3.size.toDouble
      for(j <- link._3){
        mat(j)(link._1) = p
      }

    }
    /*println(allLinks)
    for(i <- 0 to n-1){
      for(j <- 0 to n-1){
        print(" " + mat(i)(j))
      }
      println()
    }*/
    (mat, allLinks.map(x=> (x._1, x._2)))
      //allLinks
    }


}

