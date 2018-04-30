package com.vc.tutorial.algo.dynamic

/**
  * https://www.youtube.com/watch?v=IRwVmTmN6go
  */
object CuttingRodProblem {
  def main(args: Array[String]): Unit = {
    val l = 5
    val lv = Array(
      (1, 2),
      (2, 5),
      (3, 5),
      (4, 8)
    ).sortBy(_._1)

    /**
      *
      * 0 1 2 3 4 5
      * 1 0 2 4 6 8 10
      * 2 0 2 5 7 10 12
      * 3 0 2 5 7 10 12
      * 4 0 2 5 7 10 12
      *
      **/

    val arr = Array.ofDim[Int](lv.length, l + 1)
    arr.indices.foreach(i => {
      arr(i).indices.foreach(j => {
        if (j == 0) {
          arr(i)(j) = 0
        } else if (j - lv(i)._1 >= 0) {
          arr(i)(j) = Math.max(lv(i)._2 + arr(i)(j - lv(i)._1), if (i - 1 < 0) 0 else arr(i - 1)(j))
        } else {
          arr(i)(j) = if (i - 1 < 0) 0 else arr(i - 1)(j)
        }
      })
    })

    arr.indices.foreach(i => {
      println(arr(i).mkString(", "))
    })
  }
}
