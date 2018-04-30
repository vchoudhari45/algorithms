package com.vc.tutorial.algo.dynamic

/**
  * https://www.youtube.com/watch?v=s6FhG--P7z0
  */
object SubsetSum {
  def main(args: Array[String]): Unit = {
    val s = Array(2, 3, 7, 8, 10)
    val total = 11
    val arr = Array.ofDim[Boolean](s.length + 1, total + 1)

    /**
      * 0 1 2 3 4 5 6 7 8 9 10 11
      * 0 0  T F F F F F F F F F F  F
      * 1 2  T F T F F F F F F F F  F
      * 2 3  T F T T F T F F F F F  F
      * 3 7  T F T T F T F T F T T  F
      * 4 8  T F T T F T F T T F T  T
      * 5 10 T F T T F T F T T F T  T
      **/

    (0 to s.length).foreach(i => {
      arr(i)(0) = true
    })
    (1 to s.length).foreach(i => {
      (1 to total).foreach(j => {
        if (j - s(i - 1) >= 0) {
          arr(i)(j) = arr(i - 1)(j - s(i - 1)) || arr(i - 1)(j)
        } else {
          arr(i)(j) = arr(i - 1)(j)
        }
      })
    })
    println(arr(s.length)(total))
  }
}
