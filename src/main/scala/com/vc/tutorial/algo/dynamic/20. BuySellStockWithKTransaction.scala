package com.vc.tutorial.algo.dynamic

/**
  * https://www.youtube.com/watch?v=oDhu5uGq_ic
  */
object BuySellStockWithKTransaction {
  def main(args: Array[String]): Unit = {
    val sp = Array(2, 5, 7, 1, 4, 3, 1, 3)
    val k = 3

    /**
      * 0 1 2 3 4 5 6 7
      * 2 5 7 1 4 3 1 3
      * 0 0 0 0 0 0 0 0 0
      * 1 0 3
      * 2 0
      * 3 0
      **/

    val arr = Array.ofDim[Int](k + 1, sp.length)
    arr.indices.foreach(i => {
      arr(i).indices.foreach(j => {
        if (j == 0 || i == 0) arr(i)(j) = 0
        else {
          var maxProfit = -1
          /**
            * 1, 1
            *
            **/
          if (j == 1) {
            maxProfit = sp(j) - sp(j - 1)
          } else {
            (0 until j).foreach(k => {
              maxProfit = Math.max(
                maxProfit,
                sp(j) - sp(k) + arr(i - 1)(k)
              )
            })
          }
          arr(i)(j) = Math.max(arr(i)(j - 1), maxProfit)
        }
      })
    })
    arr.indices.foreach(i => {
      println(arr(i).mkString("\t"))
    })

  }
}
