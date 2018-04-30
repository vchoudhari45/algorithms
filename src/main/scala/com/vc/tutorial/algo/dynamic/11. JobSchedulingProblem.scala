package com.vc.tutorial.algo.dynamic

/**
  * https://www.youtube.com/watch?v=cr6Ip0J9izc
  */
object JobSchedulingProblem {
  def main(args: Array[String]): Unit = {
    val job = Array(
      (1, 3, 5),
      (2, 5, 6),
      (4, 6, 5),
      (6, 7, 4),
      (5, 8, 11),
      (7, 9, 2)
    ).sortBy(_._2)

    val n = job.length
    val out = job.map(_._3)
    /**
      *
      * i           j
      * (1,3) (2,5) (4,6) (6,7) (5,8) (7,9)
      * 5     6     5     4     11    2
      **/

    //(1, 3) (2, 5) (9, 10)

    var i = 0
    var j = 1
    while (j < n) {
      if (i == j) {
        j = j + 1
        i = 0
      } else {
        if (!isOverlap(job(i), job(j)) && out(j) < out(i) + job(j)._3) {
          out(j) = out(i) + job(j)._3
        }
        i = i + 1
      }
    }
    println(out.max)
  }

  def isOverlap(t1: (Int, Int, Int), t2: (Int, Int, Int)): Boolean = !(t2._1 >= t1._2)
}
