package com.vc.tutorial.algo.dynamic

/**
  * https://www.youtube.com/watch?v=YDf982Lb84o
  */
object CountTotalBSTForN {
  def main(args: Array[String]): Unit = {
    val n = 5

    val arr = new Array[Int](n + 1)
    arr(0) = 1
    arr(1) = 1

    var i = 2
    var j = 0
    while (i <= n) {
      var j = 0
      while (j < i) {
        arr(i) += arr(j) * arr(i - j - 1)
        j = j + 1
      }
      i = i + 1
    }
    println(arr(n))
  }
}
