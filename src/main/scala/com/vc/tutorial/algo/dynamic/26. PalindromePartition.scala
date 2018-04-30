package com.vc.tutorial.algo.dynamic

/**
  * https://www.youtube.com/watch?v=lDYIvtBVmgo
  */
object PalindromePartition {
  def main(args: Array[String]): Unit = {
    val s = "abcbm"

    /**
      * a b c b m
      * 0 1 2 3 4
      * 0 0 1 2 2
      * 1   0 1 2 2
      * 2     0 1 2
      * 3       0 1
      * 4         0
      *
      **/

    val n = s.length
    val arr = Array.ofDim[Int](n, n)
    var i = n
    var j = 0
    var k = 0
    while (i >= 0) {
      k = n - i
      j = 0
      while (k < n) {
        //println(s"$j $k")
        if (j == k) {
          arr(j)(k) = 0
        }
        else if (s.charAt(j) == s.charAt(k) && (k - j == 1 || arr(j + 1)(k - 1) == 0)) {
          arr(j)(k) = 0
        }
        else if (s.charAt(j) != s.charAt(k) && k - j == 1) {
          arr(j)(k) = 1
        }
        else {
          var min = Int.MaxValue
          (j until k).foreach(l => {
            min = Math.min(
              min,
              arr(j)(l) + arr(l + 1)(k) + 1
            )
          })
          arr(j)(k) = min
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
