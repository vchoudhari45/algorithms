package com.vc.tutorial.algo.dynamic

/**
  * https://www.youtube.com/watch?v=_fgjrs570YE
  */
object CoinChangingProblem {
  def main(args: Array[String]): Unit = {
    val c = Array(1, 2, 3)
    val total = 5

    /**
      * 0 1 2 3 4 5
      * 0 1 0 0 0 0 0
      * 1 1 1 1 1 1 1
      * 2 1 1 2 2 3 3
      * 3 1 1 2 3 4 5
      **/
    val arr = Array.ofDim[Int](c.length + 1, total + 1)
    arr.indices.foreach(i => {
      arr(i).indices.foreach(j => {
        if (j == 0) {
          arr(i)(j) = 1
        }
        else if (i == 0) {
          arr(i)(j) = 0
        }
        else if (j - c(i - 1) >= 0) {
          arr(i)(j) = arr(i - 1)(j) + arr(i)(j - c(i - 1))
        } else {
          arr(i)(j) = arr(i - 1)(j)
        }
      })
    })
    arr.indices.foreach(i => {
      println(arr(i).mkString(", "))
    })
  }
}
