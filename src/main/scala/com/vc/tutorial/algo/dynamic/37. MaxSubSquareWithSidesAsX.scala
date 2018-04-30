package com.vc.tutorial.algo.dynamic

/**
  * https://www.youtube.com/watch?v=vi_1eHCsR9A
  */
object MaxSubSquareWithSidesAsX {
  def main(args: Array[String]): Unit = {
    val n = 5
    val s = Array.ofDim[Char](n, n)

    s(0)(0) = 'O'
    s(0)(1) = 'O'
    s(0)(2) = 'O'
    s(0)(3) = 'O'
    s(0)(4) = 'X'

    s(1)(0) = 'X'
    s(1)(1) = 'O'
    s(1)(2) = 'X'
    s(1)(3) = 'X'
    s(1)(4) = 'X'

    s(2)(0) = 'X'
    s(2)(1) = 'O'
    s(2)(2) = 'X'
    s(2)(3) = 'O'
    s(2)(4) = 'X'

    s(3)(0) = 'X'
    s(3)(1) = 'X'
    s(3)(2) = 'X'
    s(3)(3) = 'X'
    s(3)(4) = 'X'

    s(4)(0) = 'O'
    s(4)(1) = 'O'
    s(4)(2) = 'X'
    s(4)(3) = 'X'
    s(4)(4) = 'X'

    /**
      * 0 0 0 0 X
      * X 0 X X X
      * X 0 X 0 X
      * X X X X X
      * 0 0 X X X
      **/
    val arr = Array.ofDim[(Int, Int)](n, n)
    s.indices.foreach(i => {
      s(i).indices.foreach(j => {
        if (s(i)(j) == 'O') {
          arr(i)(j) = (0, 0)
        }
        else {
          arr(i)(j) = (
            if (i - 1 < 0) 1 else arr(i - 1)(j)._1 + 1,
            if (j - 1 < 0) 1 else arr(i)(j - 1)._2 + 1
          )
        }
      })
    })

    arr.indices.foreach(i => {
      println(arr(i).mkString(" "))
    })

    var maxArea = 0
    var i = n - 1
    var j = n - 1
    while (i >= 0) {
      while (j >= 0) {
        val e = arr(i)(j)
        if (e._1 > e._2 && maxArea < e._2) {
          var area = e._2
          while (area > 0 && maxArea < area) {
            if (
              i - area + 1 >= 0 && arr(i - area + 1)(j)._2 >= area &&
                j - area + 1 >= 0 && arr(i)(j - area + 1)._1 >= area
            ) {
              maxArea = area
              area = 0
            }
            else {
              area = area - 1
            }
          }
        }
        else if (e._2 > e._1 && maxArea < e._1) {
          var area = e._1
          while (area > 0 && maxArea < area) {
            if (
              i - area + 1 >= 0 && arr(i - area + 1)(j)._2 >= area &&
                j - area + 1 >= 0 && arr(i)(j - area + 1)._1 >= area
            ) {
              maxArea = area
              area = 0
            }
            else {
              area = area - 1
            }
          }
        }
        j = j - 1
      }
      j = n - 1
      i = i - 1
    }
    println(maxArea)
  }
}
