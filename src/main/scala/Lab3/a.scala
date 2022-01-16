package Lab3
import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}
import org.jsoup.select.Elements

import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.util.control.Breaks.break



object Ex44 {
  def main(args: Array[String]): Unit = {//.select(".mediawiki ltr sitedir-ltr mw-hide-empty-elt ns-0 ns-subject " +
    //"page-Main_Page rootpage-Main_Page skin-vector action-view skin-vector-legacy")
    val doc = Jsoup.connect("https://en.wikipedia.org/").get()
    var firstLinks : Elements  = doc.select("a[href^=/wiki/]")
    var links = firstLinks.select("a:not([class])")
    println(links.size)
    //var mainPageLinks = Map[Int, Element]()
    //ID, flag, link, out, int
    var allLinks = List[(Int, Int, String, ListBuffer[Int], ListBuffer[Int])]()
    var i = 0
    val n = 100
    for(el <- links.asScala){
      if(!el.attr("href").matches("/wiki/\\w+:\\w+")){
        i +=1
        if(i < n){
          allLinks :+= (i, 1, el.attr("href"), ListBuffer[Int](), ListBuffer[Int]())
        }
        else{
          allLinks :+= (i, 0, el.attr("href"), ListBuffer[Int](), ListBuffer[Int]())
        }
      }
    }
    println(allLinks.size)
    if(i >= n){
      for(link <- allLinks){
        val doc = Jsoup.connect("https://en.wikipedia.org" + link._3).get()
        val firstLinks : Elements  = doc.select("a[href^=/wiki/]")
        val links = firstLinks.select("a:not([class])")
        for(el <- links.asScala){
          val newLink = el.attr("href")
          if(!el.attr("href").matches("/wiki/\\w+:\\w+")){
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
    else{
      while(i < n){
        for(link <- allLinks if link._2 == 1){
          val doc = Jsoup.connect("https://en.wikipedia.org" + link._3).get()
          val firstLinks : Elements  = doc.select("a[href^=/wiki/]")
          val links = firstLinks.select("a:not([class])")
          for(el <- links.asScala){
            val newLink = el.attr("href")
            if(!el.attr("href").matches("//wiki/\\w+:\\w+/g")){
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
    var linksMap = Map[Int, List[Int]]()
    allLinks.foreach(x=> {if(x._2 == 1){println(x)}})


  }


  //def CrawlMode(n : Int) : Unit={}
  //def PageRankMode(n : Int) : Unit={}
  //def LinkAnalysisMode(n : Int) : Unit={}
}

