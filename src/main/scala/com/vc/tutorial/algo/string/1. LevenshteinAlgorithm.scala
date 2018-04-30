package com.vc.tutorial.algo.string

/**
  * Dynamic programing Algorithm
  * https://www.youtube.com/watch?v=We3YDTzNXEk
  * Time complexity o(m * n)
  */
object LevenshteinAlgorithm {

  def main(args: Array[String]): Unit = {
    println(getMinimumDistance("POON", "POIE"))
  }

  def getMinimumDistance(s1: String, s2: String): Int = {

    val row = s1.length + 1
    val col = s2.length + 1

    if (s1.isEmpty) return col - 1
    if (s2.isEmpty) return row - 1

    val matrix = new Array[Array[Int]](row)
    (0 until col).indices.foreach(i => {
      matrix(i) = new Array[Int](row)
      matrix(i)(0) = i
    })

    (0 until row).indices.foreach(i => {
      matrix(0)(i) = i
    })

    (1 until row).foreach(i => {
      (1 until col).foreach(j => {
        if (s1.charAt(i - 1) == s2.charAt(j - 1)) {
          matrix(i)(j) = matrix(i - 1)(j - 1)
        } else {
          matrix(i)(j) = Math.min(Math.min(matrix(i - 1)(j), matrix(i - 1)(j - 1)), matrix(i)(j - 1)) + 1
        }
      })
    })
    matrix(row - 1)(col - 1)
  }
}
