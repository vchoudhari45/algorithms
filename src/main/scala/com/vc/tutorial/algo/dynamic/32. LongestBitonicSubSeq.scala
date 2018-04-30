package com.vc.tutorial.algo.dynamic

/**
  * https://www.youtube.com/watch?v=TWHytKnOPaQ
  */
object LongestBitonicSubSeq {
  def main(args: Array[String]): Unit = {
    val s = Array(2, -1, 4, 3, 5, -1, 3, 2)
    val a1 = longestSubSeq(s)
    val a2 = longestSubSeq(s.reverse)
    val a3 = for {
      i <- s.indices
    } yield {
      a1(i) + a2(i)
    }
    println(s"longest bitonic subseq: ${a3.max}")
  }

  def longestSubSeq(arr: Array[Int]): Array[Int] = {
    var i = 0
    var j = 1
    val n = arr.length
    val out = Array.fill(n)(1)
    while (j < n) {
      i = 0
      while (i < j) {
        if (arr(j) > arr(i)) {
          out(j) = Math.max(out(i) + 1, out(j))
        }
        i = i + 1
      }
      j = j + 1
    }
    out
  }
}
