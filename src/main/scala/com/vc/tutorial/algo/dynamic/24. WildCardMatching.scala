package com.vc.tutorial.algo.dynamic

/**
  * https://www.youtube.com/watch?v=3ZDZ-N0EPV0
  */
object WildCardMatching {
  def main(args: Array[String]): Unit = {
    /**
      * '*'   0 Or more character
      * '?'   any one character
      **/

    val p = "x?y*z"
    val s = "xaylmz"

    /**
      *
      * j
      * 0 x ? y * z
      * 0 T F F F F F
      * x F T F F F F
      * a F F T F F F
      * i y F F F T T F
      * l F F F F T F
      * m F F F F T F
      * z F
      *
      **/
    val arr = Array.ofDim[Boolean](s.length + 1, p.length + 1)
    arr.indices.foreach(i => {
      arr(i).indices.foreach(j => {
        if (i == 0 && j == 0) {
          arr(i)(j) = true
        }
        else if (i == 0 || j == 0) {
          arr(i)(j) = false
        }
        else if (p.charAt(j - 1) == '*') {
          arr(i)(j) = arr(i - 1)(j) || arr(i)(j - 1) || arr(i - 1)(j - 1)
        }
        else if (p.charAt(j - 1) == s.charAt(i - 1) || p.charAt(j - 1) == '?') {
          arr(i)(j) = arr(i - 1)(j - 1)
        }
      })
    })

    arr.indices.foreach(i => {
      arr(i).indices.foreach(j => {
        if (arr(i)(j)) print("T ")
        else print("F ")
      })
      println()
    })

  }
}
