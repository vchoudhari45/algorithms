package com.vc.tutorial.algo.dynamic

/**
  * https://www.youtube.com/watch?v=hgA4xxlVvfQ
  * value: 10 12 16 21
  * freq:   4  2  6  3
  */
object OptimalBinarySearch {
  def main(args: Array[String]): Unit = {
    val v = Array(10, 12, 16, 21)
    val f = Array(4, 2, 6, 3)
    val n = f.length
    val arr = Array.ofDim[Int](n, n)

    /**
      * 0, 0
      * 1, 1
      * 2, 2
      * 3, 3
      *
      * 0, 1
      * 1, 2
      * 2, 3
      *
      * 0, 2
      * 1, 3
      *
      * 0, 3
      **/

    /**
      * 0 1 2 3
      * 0 4
      * 1   2
      * 2     6
      * 3       3
      **/
    var i = n
    while (i != 0) {
      var j = 0
      var k = n - i
      while (j < i) {
        if (j == k) {
          arr(j)(k) = f(j)
        }
        else {
          var sum = 0
          var min = Integer.MAX_VALUE
          if (k - j == 1) {
            arr(j)(k) = arr(j)(j) + arr(k)(k) + Math.min(arr(j)(j), arr(k)(k))
          } else {
            (j to k).foreach(l => {
              sum += arr(l)(l)
              min = Math.min(
                min,
                (if (l - 1 < 0) 0 else arr(j)(l - 1)) +
                  (if (l + 1 > n - 1) 0 else arr(l + 1)(k))
              )
            })
            arr(j)(k) = sum + min
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
