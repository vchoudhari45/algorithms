package com.vc.tutorial.algo.dynamic

/**
  * https://www.youtube.com/watch?v=BysNXJHzCEs
  */
object LongestCommonSubStr {
  def main(args: Array[String]): Unit = {
    val s1 = "abcdaf"
    val s2 = "zbcdf"

    /** a b c d a f
      * 0 1 2 3 4 5 6
      * 0 0 0 0 0 0 0 0
      * z 1 0 0 0 0 0 0 0
      * b 2 0 0 1 0 0 0 0
      * c 3 0 0 0 2 0 0 0
      * d 4 0 0 0 0 3 0 0
      * f 5 0 0 0 0 0 0 1
      * */

    val arr = Array.ofDim[Int](s1.length + 1, s2.length + 1)
    var lcs = 0
    arr.indices.foreach(i => {
      arr(i).indices.foreach(j => {
        if (i == 0 || j == 0) arr(i)(j) = 0
        else if (s1.charAt(i - 1) == s2.charAt(j - 1)) {
          arr(i)(j) = arr(i - 1)(j - 1) + 1
          lcs = Math.max(lcs, arr(i)(j))
        } else {
          arr(i)(j) = 0
        }
      })
    })
    println(lcs)
    arr.indices.foreach(i => {
      println(arr(i).mkString(", "))
    })
  }
}
