package com.vc.tutorial.algo.dynamic

/**
  * https://www.youtube.com/watch?v=WxpIHvsu1RI
  */
object OptimalStrategyGame {
  def main(args: Array[String]): Unit = {
    val s = Array(3, 9, 1, 2)

    /**
      * 0     1      2     3
      * 0  (3,0) (9,3) (4,9) (11,4)
      * 1        (9,0) (9,1) (10,2)
      * 2              (1,0) (2,1)
      * 3                    (2,0)
      *
      **/
    val n = s.length
    val arr = Array.ofDim[(Int, Int)](n, n)
    var i = n
    var j = 0
    var k = 0
    while (i >= 0) {
      j = 0
      k = n - i
      while (k < n) {
        //println(s"$j $k")
        if (j == k) {
          arr(j)(k) = (s(j), 0)
        }
        else {
          var max = 0
          val first = s(j) + arr(j + 1)(k)._2
          if (max < first) {
            max = first
            arr(j)(k) = (first, arr(j + 1)(k)._1)
          }
          val last = s(k) + arr(j)(k - 1)._2
          if (max < last) {
            max = last
            arr(j)(k) = (last, arr(j)(k - 1)._1)
          }
        }
        j = j + 1
        k = k + 1
      }
      i = i - 1
    }

    arr.indices.foreach(i => {
      println(arr(i).mkString(" "))
    })
  }
}
