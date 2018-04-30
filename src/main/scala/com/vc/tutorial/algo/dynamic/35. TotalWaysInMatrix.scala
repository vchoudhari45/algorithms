package com.vc.tutorial.algo.dynamic

/**
  * https://www.youtube.com/watch?v=GO5QHC_BmvM
  */
object TotalWaysInMatrix {
  def main(args: Array[String]): Unit = {
    val n = 4
    val arr = Array.ofDim[Int](n, n)

    /**
      * 1 1 1 1
      * 1 2 3 4
      * 1 3 6 10
      * 1 4 10 20
      **/

    arr.indices.foreach(i => {
      arr(i).indices.foreach(j => {
        if (i == 0 || j == 0) arr(i)(j) = 1
        else {
          arr(i)(j) = arr(i - 1)(j) + arr(i)(j - 1)
        }
      })
    })

    arr.indices.foreach(i => {
      println(arr(i).mkString(" "))
    })

  }
}
