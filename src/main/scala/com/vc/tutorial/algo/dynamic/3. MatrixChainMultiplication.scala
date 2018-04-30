package com.vc.tutorial.algo.dynamic

/**
  * https://www.youtube.com/watch?v=vgLJZMUfnsU
  */
object MatrixChainMultiplication {
  def main(args: Array[String]): Unit = {
    /**
      * 0, 0
      * 1, 1
      * 2, 2
      * 3, 3
      * 4, 4
      * 5, 5
      *
      * 0, 1
      * 1, 2
      * 2, 3
      * 3, 4
      * 4, 5
      *
      * 0, 2
      * 1, 3
      * 2, 4
      * 3, 5
      *
      * 0, 3
      * 1, 4
      * 2, 5
      *
      * 0, 4
      * 1, 5
      *
      * 0, 5
      *
      **/

    val m = Array(2, 3, 6, 4, 5)
    val n = m.length - 1
    val arr = Array.ofDim[Int](n, n)

    /**
      * 0 1 2 3
      * 0 0
      * 1   0
      * 2     0
      * 3       0
      **/
    var i = n
    while (i != 0) {
      var j = 0
      var k = n - i
      while (j < i) {
        //println(s"$j $k")
        if (j == k) arr(j)(k) = 0
        else if (k - j == 1) {
          arr(j)(k) = m(j) * m(j + 1) * m(k + 1)
        } else {
          var cost = 0
          var min = Integer.MAX_VALUE
          /**
            * 0 to 3
            * (0, 0) * (1, 3) l = 0
            * (0, 1) * (2, 3) l = 1
            * (0, 2) * (3, 3) l = 2
            *
            * 0 to 2
            * (0, 0) * (1, 2) l = 0
            * (0, 1) * (2, 2) l = 1
            *
            * 2, 3, 6, 4, 5
            * [2, 3] [3, 6] [6, 4] [4, 5]
            * 0      1      2      3
            **/
          (j until k).foreach(l => {
            if (j == l) {
              cost = arr(l + 1)(k) + m(j) * m(j + 1) * m(k + 1)
            } else if (k == l + 1) {
              cost = arr(j)(l) + m(k) * m(k + 1) * m(j)
            } else {
              cost = arr(j)(l) + arr(l)(k) + m(j) * m(l + 1) * m(k + 1)
            }
            min = Math.min(cost, min)
          })
          arr(j)(k) = min
        }
        j = j + 1
        k = k + 1
      }
      i = i - 1
    }

    arr.indices.foreach(i => {
      println(arr(i).mkString(", "))
    })

  }
}
