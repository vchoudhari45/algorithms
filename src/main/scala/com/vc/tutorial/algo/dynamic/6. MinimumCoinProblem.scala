package com.vc.tutorial.algo.dynamic

/**
  * https://www.youtube.com/watch?v=NJuKJ8sasGk
  */
object MinimumCoinProblem {
  def main(args: Array[String]): Unit = {
    val c = Array(7, 2, 3, 6).sorted
    val total = 13
    val arr = Array.ofDim[Int](c.length + 1, total + 1)

    /**
      * 0 1 2 3 4 5 6 7 8 9 10 11 12 13
      * 0 0 0 I I I I I I I I I I  I  I  I
      * 1 2 0 I 1 I I I I I I I I  I  I  I
      * 2 3 0 I 1 1 I 2 I I I I I  I  I  I
      * 3 6 0 I 1 1 I 2 1 I 2 2 I  3  I  I
      * 4 7 0 I 1 1 I 2 1 1 2 2 2  3  3  2
      **/

    arr.indices.foreach(i => {
      arr(i).indices.foreach(j => {
        if (j == 0)
          arr(i)(j) = 0
        else if (i == 0)
          arr(i)(j) = Integer.MAX_VALUE
        else if (j - c(i - 1) >= 0 && arr(i - 1)(j - c(i - 1)) != Integer.MAX_VALUE) {
          arr(i)(j) = Math.min(1 + arr(i - 1)(j - c(i - 1)), arr(i - 1)(j))
        } else {
          arr(i)(j) = arr(i - 1)(j)
        }
      })
    })

    arr.indices.foreach(i => {
      arr(i).indices.foreach(j => {
        if (arr(i)(j) != Integer.MAX_VALUE)
          print(arr(i)(j) + " ")
        else
          print("I ")
      })
      println()
    })
  }
}
