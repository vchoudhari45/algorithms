package com.vc.tutorial.algo.dynamic

/**
  * https://www.youtube.com/watch?v=l3hda49XcDE
  */
object RegularExpression {
  def main(args: Array[String]): Unit = {
    /**
      * * is zero of more occurrences of character before *
      * . is single occurrences any character
      **/
    val e = "xa*b.c"
    val p = "xaabyc"

    /**
      * 0 1 2 3 4 5 6
      * 0 x a * b . c
      * 0 0 T F F F F F F
      * 1 x F T F T F F F
      * 2 a F F T T F F F
      * 3 a F F F
      * 4 b F
      * 5 y F
      * 6 c F
      *
      **/
    val arr = Array.ofDim[Boolean](e.length + 1, p.length + 1)
    arr.indices.foreach(i => {
      arr(i).indices.foreach(j => {
        if (i == 0 && j == 0) arr(i)(j) = true
        else if (i == 0) {
          if (p.startsWith("*") && j == 1) arr(i)(j) = true
          else arr(i)(j) = false
        }
        else if (j == 0) {
          arr(i)(j) = false
        }
        else if (e.charAt(j - 1) == '*') {
          /**
            * x a *
            * x T F T
            * a F T T
            * a F F T
            *
            * x a *
            * x T F T
            * b F F F
            **/
          arr(i)(j) = arr(i)(j - 2) || arr(i)(j - 1) || arr(i - 1)(j - 1)
        }
        else if (e.charAt(j - 1) == p.charAt(i - 1) || e.charAt(j - 1) == '.') {
          arr(i)(j) = arr(i - 1)(j - 1)
        }
        else {
          arr(i)(j) = false
        }
      })
    })

    println(arr(e.length)(p.length))

    arr.indices.foreach(i => {
      arr(i).indices.foreach(j => {
        if (arr(i)(j))
          print("T ")
        else
          print("F ")
      })
      println()
    })
  }
}
