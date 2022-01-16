package Lab2

import scala.collection.mutable.ListBuffer
import scala.io.Source
import java.io._
 

object Ex1 {
  def main(args : Array[String]) : Unit = {
    val input_string = "D:\\Studia\\Magisterskie\\II semestr\\Big Data Algorithms\\Laboratorium\\Lab0_1\\files\\webStanford.txt"
    val (vi, vo) = Map1(input_string)
    println("Map1")
    //val (vi, vo) = Map2(ve)
    //println("Map2")
    val vertices = Reduce(vi, vo)
    println("Reduce")
    val pw = new PrintWriter(new File("output.txt"))
    pw.write("Vertice, In, Out\n")
    for(v <- vertices){
      pw.write(v.toString()+'\n')
    }

  }
  def Map1(input:String) : (Map[Int,ListBuffer[Int]], Map[Int,ListBuffer[Int]]) = {
    var vi : Map[Int, ListBuffer[Int]] = Map()
    var vo : Map[Int, ListBuffer[Int]] = Map()
    var lines = Source.fromFile(input).getLines()
    for (line <- lines) {
      val tup = line.split("\t") match { case Array(x1, x2) => (x1.toInt, x2.toInt) }
      if(vo.contains(tup._1)){
        vo(tup._1) -> (vo(tup._1) += tup._2)
      }
      else{
        vo += (tup._1 -> ListBuffer(tup._2))
      }
      if(vi.contains(tup._2)){
        vi(tup._2) -> (vi(tup._2)+=tup._1)
      }
      else{
        vi += (tup._2 -> ListBuffer(tup._1))
      }
    }
    return (vi, vo)
  }
    def Map2(ve:List[(Int,Int)]) : (Map[Int,ListBuffer[Int]], Map[Int,ListBuffer[Int]]) = {
      var vi : Map[Int, ListBuffer[Int]] = Map()
      var vo : Map[Int, ListBuffer[Int]] = Map()
      for(v <- ve){
        if(vo.contains(v._1)){
          vo(v._1) -> (vo(v._1) += v._2)
        }
        else{
          vo += (v._1 ->ListBuffer(v._2))
        }
        if(vi.contains(v._2)){
            vi(v._2) -> (vi(v._2) += v._1)
        }
        else{
            vi += (v._2 -> ListBuffer(v._1))
        }
      }
      return (vi, vo)
    }
  def Reduce(vi : Map[Int, ListBuffer[Int]], vo : Map[Int, ListBuffer[Int]]): List[(Int, Int, Int)] ={
    var out = List[(Int, Int, Int)]()
    var vert = vi.keys.toSet union vo.keys.toSet
    var srt_vert = vert.toSeq.sorted
    for(v <- vert){
      var inDeg = 0
      var outDeg = 0
      if(vi.contains(v))
        inDeg = vi(v).size
      if(vo.contains(v))
        outDeg = vo(v).size
      //println(v, inDeg, outDeg)
      out :+= (v, inDeg, outDeg)
    }
    return out.sortBy(x=>x._1)
  }
}
