package com.vc.tutorial.algo.dynamic

/**
  * https://www.youtube.com/watch?v=NnD96abizww
  */
object LongestCommonSubSeq {
  def main(args: Array[String]): Unit = {
    val s1 = "abcdaf"
    val s2 = "acbcf"

    val arr = Array.ofDim[Int](s1.length + 1, s2.length + 1)
    arr.indices.foreach(i => {
      arr(i).indices.foreach(j => {
        if (i == 0 || j == 0) arr(i)(j) = 0
        else if (s1.charAt(i - 1) == s2.charAt(j - 1)) {
          arr(i)(j) = arr(i - 1)(j - 1) + 1
        } else {
          arr(i)(j) = Math.max(arr(i - 1)(j), arr(i)(j - 1))
        }
      })
    })

    var i = s1.length
    var j = s2.length
    while (i != 0 && j != 0) {
      if (arr(i)(j) == arr(i - 1)(j)) i = i - 1
      else if (arr(i)(j) == arr(i)(j - 1)) j = j - 1
      else {
        print(s1.charAt(i - 1) + " ")
        i = i - 1
        j = j - 1
      }
    }
    println()
    println(arr(s1.length)(s2.length))
  }
}
