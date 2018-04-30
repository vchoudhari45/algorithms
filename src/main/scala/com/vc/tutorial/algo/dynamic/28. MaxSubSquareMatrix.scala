package com.vc.tutorial.algo.dynamic

/**
  * https://www.youtube.com/watch?v=_Lf1looyJMU
  */
object MaxSubSquareMatrix {
  def main(args: Array[String]): Unit = {
    val arr = Array.ofDim[Int](4, 5)

    arr(0)(0) = 0
    arr(0)(1) = 0
    arr(0)(2) = 1
    arr(0)(3) = 1
    arr(0)(4) = 1

    arr(1)(0) = 1
    arr(1)(1) = 0
    arr(1)(2) = 1
    arr(1)(3) = 1
    arr(1)(4) = 1

    arr(2)(0) = 0
    arr(2)(1) = 1
    arr(2)(2) = 1
    arr(2)(3) = 1
    arr(2)(4) = 1

    arr(3)(0) = 1
    arr(3)(1) = 0
    arr(3)(2) = 1
    arr(3)(3) = 1
    arr(3)(4) = 1

    /**
      * 0 0 1 1 1
      * 1 0 1 1 1
      * 0 1 1 1 1
      * 1 0 1 1 1
      **/

    val r = arr.length
    val c = arr(0).length
    val out = Array.ofDim[Int](r + 1, c + 1)
    out.indices.foreach(i => {
      out(i).indices.foreach(j => {
        if (i == 0 || j == 0) {
          out(i)(j) = 0
        } else if (arr(i - 1)(j - 1) == 0) {
          out(i)(j) = 0
        } else {
          out(i)(j) = 1 +
            Math.min(
              Math.min(
                out(i - 1)(j - 1),
                out(i - 1)(j)
              ),
              out(i)(j - 1)
            )
        }
      })
    })

    out.indices.foreach(i => {
      println(out(i).mkString(" "))
    })
  }
}
