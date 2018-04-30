package com.vc.tutorial.algo.string

/**
  * LCP Array:
  * Longest Common prefix between consecutive elements(AKA suffixes) of [[SuffixArray]]
  *
  * For String banana
  * Suffixes represented by suffix array in order are:
  * {"a", "ana", "anana", "banana", "na", "nana"}
  *
  * lcp[0] = Longest Common Prefix of "a" and "ana" = 1
  * lcp[1] = Longest Common Prefix of "ana" and "anana" = 3
  * ... etc
  *
  * http://www.geeksforgeeks.org/%C2%AD%C2%ADkasais-algorithm-for-construction-of-lcp-array-from-suffix-array/
  */
object LCPArray {

  def main(args: Array[String]): Unit = {
    println(kasai("banana").mkString(", "))
  }
  def kasai(s: String): Array[Int] = {
    val sArr = SuffixArray.buildSuffixArray(s.toCharArray)
    val invSuffixArray = new Array[Int](sArr.length)
    val n = sArr.length
    val lcp = new Array[Int](n)

    sArr.indices.foreach(i => {
      invSuffixArray(sArr(i)) = i
    })
    var k = 0

    sArr.indices.foreach(i => {
      scala.util.control.Breaks.breakable {
        if (invSuffixArray(i) == n - 1) {
          k = 0
          scala.util.control.Breaks.break
        }
        val j = sArr(invSuffixArray(i) + 1)
        while (i + k < n && j + k < n && s.charAt(i + k) == s.charAt(j + k)) k = k + 1
        lcp(invSuffixArray(i)) = k
        if (k > 0) k = k - 1
      }
    })
    lcp
  }
}
