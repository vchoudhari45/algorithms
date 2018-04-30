package com.vc.tutorial.algo.dynamic

/**
  * https://www.youtube.com/watch?v=_nCsPn7_OgI
  */
object LongestPalindromicSubSeq {
  def main(args: Array[String]): Unit = {
    val s1 = "agbdba"
    val n = s1.length - 1

    val arr = Array.ofDim[Int](s1.length, s1.length)
    /**
      * 0,0
      * 1,1
      * 2,2
      * 3,3
      * 4,4
      * 5,5
      *
      * 0,1
      * 1,2
      * 2,3
      * 3,4
      * 4,5
      *
      * 0,2
      * 1,3
      * 2,4
      * 3,5
      *
      * 0,3
      * 1,4
      * 2,5
      *
      * 0,4
      * 1,5
      *
      * 0,5
      *
      * a g b d b a
      * 0 1 2 3 4 5
      *
      * 0 1 2 3 4 5
      * 0 1 1 1 1 3 5
      * 1   1 1 1 3 3
      * 2     1 1 3 3
      * 3       1 1 1
      * 4         1 1
      * 5           1
      **/


    var i = n
    var j = 0
    var k = 0
    while (i >= 0) {
      j = 0
      k = n - i
      while (k <= n) {
        //println(s"$j $k")
        if (j == k) arr(j)(k) = 1
        else if (s1.charAt(j) == s1.charAt(k)) {
          arr(j)(k) = 2 + arr(j + 1)(k - 1)
        } else {
          arr(j)(k) = Math.max(arr(j)(k - 1), arr(j + 1)(k))
        }
        j = j + 1
        k = k + 1
      }
      i = i - 1
    }
    arr.indices.foreach(i => {
      println(arr(i).mkString(", "))
    })
    println(s"Ans: ${arr(0)(n)}")
  }
}
