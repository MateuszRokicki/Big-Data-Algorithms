package Lab2

import scala.collection.mutable.ListBuffer
import scala.io.Source
 

object Ex1 {
  def main(args : Array[String]) : Unit = {
    val input_string = "D:\\Studia\\Magisterskie\\II semestr\\Big Data Algorithms\\Laboratorium\\Lab0_1\\files\\webStanford.txt"
    val ve = Map1(input_string)
    println("Map1")
    val (vi, vo) = Map2(ve)
    println("Map2")
    val vertices = Reduce(vi, vo)
    println("Reduce")
    for(v <- vertices){
      println(v)
    }

  }
  def Map1(input:String) : List[(Int, Int)] = {
    var ve = List[(Int, Int)]()
    var lines = Source.fromFile(input).getLines()
    var i = 0
    for (line <- lines) {
      val tup = line.split("\t") match { case Array(x1, x2) => (x1.toInt, x2.toInt) }
      println(i)
      ve :+= tup
      i += 1
    }
    return ve.sortBy(_._1)
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
    for(v <- vert){
      var inDeg = 0
      var outDeg = 0
      if(vi.contains(v))
        inDeg = vi(v).size
      if(vo.contains(v))
        outDeg = vo(v).size
      out :+= (v, inDeg, outDeg)
    }
    return out.sortBy(x=>x._1)
  }
}
