package com.vc.tutorial.algo.dynamic

import scala.collection.mutable

/**
  * https://www.youtube.com/watch?v=lBRtnuxg-gU
  */
object CostPath {
  def main(args: Array[String]): Unit = {
    val arr = Array.ofDim[Int](3, 4)

    arr(0)(0) = 1
    arr(0)(1) = 3
    arr(0)(2) = 5
    arr(0)(3) = 8

    arr(1)(0) = 4
    arr(1)(1) = 2
    arr(1)(2) = 1
    arr(1)(3) = 7

    arr(2)(0) = 4
    arr(2)(1) = 3
    arr(2)(2) = 2
    arr(2)(3) = 3

    arr.indices.foreach(i => {
      println(arr(i).mkString(" "))
    })

    val r = arr.length
    val c = if (arr(0) == null) 0 else arr(0).length
    val arrOut = Array.ofDim[Int](r, c)


    /**
      * 1 3 5 8
      * 4 2 1 7
      * 4 3 2 3
      **/

    (0 until c).foreach(i => {
      arrOut(0)(i) = if (i - 1 < 0) arr(0)(0) else arrOut(0)(i - 1) + arr(0)(i)
    })

    (0 until r).foreach(i => {
      arrOut(i)(0) = if (i - 1 < 0) arr(0)(0) else arrOut(i - 1)(0) + arr(i)(0)
    })

    (1 until r).foreach(i => {
      (1 until c).foreach(j => {
        arrOut(i)(j) = Math.min(arrOut(i - 1)(j), arrOut(i)(j - 1)) + arr(i)(j)
      })
    })

    println()
    arrOut.indices.foreach(i => {
      println(arrOut(i).mkString(" "))
    })

    val st = new mutable.Stack[Int]()
    var i = r - 1
    var j = c - 1
    st.push(arr(i)(j))
    while (i > 0 || j > 0) {
      if (i > 0 && j > 0 && arr(i - 1)(j) > arr(i)(j - 1)) {
        j = j - 1
        st.push(arr(i)(j))
      } else if (i > 0 && j > 0 && arr(i - 1)(j) <= arr(i)(j - 1)) {
        i = i - 1
        st.push(arr(i)(j))
      } else {
        if (i > 0) {
          while (i > 0) {
            i = i - 1
            st.push(arr(i)(j))

          }
        } else {
          while (j > 0) {
            j = j - 1
            st.push(arr(i)(j))
          }
        }
      }
    }
    println(st.mkString(" -> "))
  }
}
