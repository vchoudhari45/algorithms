package com.vc.tutorial.algo.dynamic

/**
  * https://www.youtube.com/watch?v=UtGtF6nc35g
  */
object MaxSubSeqNonAdj {
  def main(args: Array[String]): Unit = {
    val s = Array(4, 1, 1, 4, 2, 1)
    var in = 0
    var ex = 0
    s.indices.foreach(i => {
      val temp = in
      in = Math.max(in, ex + s(i))
      ex = temp
    })
    println(s"MaxSum: $in")
  }
}
