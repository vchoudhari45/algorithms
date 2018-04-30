package com.vc.tutorial.algo.dynamic

/**
  * https://www.youtube.com/watch?v=We3YDTzNXEk
  */
object MinimumEditDistance {
  def main(args: Array[String]): Unit = {
    val s1 = "abcdef"
    val s2 = "azced"
    val arr = Array.ofDim[Int](s1.length + 1, s2.length + 1)
    /** *     0 1 2 3 4 5 6
      * / a b c d e f
      * 0 / 0 1 2 3 4 5 6
      * 1 a 1 0
      * 2 z 2
      * 3 c 3
      * 4 e 4
      * 5 d 5
      */

    arr.indices.foreach(i => {
      arr(i).indices.foreach(j => {
        if (i == 0) arr(i)(j) = j
        else if (j == 0) arr(i)(j) = i
        else {
          if (s1.charAt(i - 1) == s2.charAt(j - 1)) {
            arr(i)(j) = arr(i - 1)(j - 1)
          }
          else {
            arr(i)(j) = Math.min(Math.min(arr(i - 1)(j - 1), arr(i - 1)(j)), arr(i)(j - 1)) + 1
          }
        }
      })
    })
    println(arr(s1.length)(s2.length))
  }
}
