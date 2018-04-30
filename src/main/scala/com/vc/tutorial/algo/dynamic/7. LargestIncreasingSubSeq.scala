package com.vc.tutorial.algo.dynamic

/**
  * https://www.youtube.com/watch?v=CE2b_-XfVDk
  */
object LargestIncreasingSubSeq {
  def main(args: Array[String]): Unit = {
    val s = Array(3, 4, -1, 0, 6, 2, 3)
    val n = s.length
    val max = Array.fill(n)(1)
    /**
      * 0 1  2 3 4 5 6
      * 3 4 -1 0 6 2 3
      * 1 1  1 1 1 1 1
      * j i
      * 1 2  1 1 1 1 1
      * j    i
      * 1 2  1 1 1 1 1
      * j  i
      * 1 2  1 1 1 1 1
      * j      i
      * 1 2  1 1 1 1 1
      * j    i
      * 1 2  1 1 1 1 1
      **/
    var j = 0
    var i = 1
    var l = 1
    var idx = 0
    while (i < n) {
      while (j < i) {
        if (s(j) < s(i)) {
          max(i) = Math.max(max(j) + 1, max(i))
          if (l < max(i)) {
            l = max(i)
            idx = i
          }
        }
        j = j + 1
      }
      j = 0
      i = i + 1
    }
    println(s"maxLength:$l idx:$idx")
  }
}
