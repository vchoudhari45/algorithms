package com.vc.tutorial.algo.dynamic

/**
  * https://www.youtube.com/watch?v=8LusJS5-AGo
  */
object KnapsackProblem {
  def main(args: Array[String]): Unit = {
    val wt = Array(1, 3, 4, 5)
    val vl = Array(1, 4, 5, 7)
    val total = 7

    val arr = Array.ofDim[Int](wt.length, total + 1)
    arr.indices.foreach(i => {
      arr(i).indices.foreach(j => {
        if (j == 0 || i == 0) arr(i)(j) = 0
        else if (j - wt(i) >= 0) {
          arr(i)(j) = Math.max(arr(i - 1)(j), vl(i) + arr(i - 1)(j - wt(i)))
        }
        else {
          arr(i)(j) = arr(i - 1)(j)
        }
      })
    })
    println(arr(wt.length - 1)(total))
  }
}
