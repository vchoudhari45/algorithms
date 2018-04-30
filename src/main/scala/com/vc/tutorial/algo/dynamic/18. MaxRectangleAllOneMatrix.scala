package com.vc.tutorial.algo.dynamic

import scala.collection.mutable

/**
  * https://www.youtube.com/watch?v=g8bSdXCG-lA
  */
object MaxRectangleAllOneMatrix {
  def main(args: Array[String]): Unit = {

    val arr = Array.ofDim[Int](4, 6)

    arr(0)(0) = 1
    arr(0)(1) = 0
    arr(0)(2) = 0
    arr(0)(3) = 1
    arr(0)(4) = 1
    arr(0)(5) = 1

    arr(1)(0) = 1
    arr(1)(1) = 0
    arr(1)(2) = 1
    arr(1)(3) = 1
    arr(1)(4) = 0
    arr(1)(5) = 1

    arr(2)(0) = 0
    arr(2)(1) = 1
    arr(2)(2) = 1
    arr(2)(3) = 1
    arr(2)(4) = 1
    arr(2)(5) = 1

    arr(3)(0) = 0
    arr(3)(1) = 0
    arr(3)(2) = 1
    arr(3)(3) = 1
    arr(3)(4) = 1
    arr(3)(5) = 1

    /**
      * 1 0 0 1 1 1
      * 1 0 1 1 0 1
      * 0 1 1 1 1 1
      * 0 0 1 1 1 1
      **/
    val r = arr.length
    val c = arr(0).length
    val sArr = Array.fill(c)(0)
    var i = 0
    var j = 0
    var maxArea = 0
    while (i < r) {
      j = 0
      while (j < c) {
        if (arr(i)(j) == 0) sArr(j) = 0
        else sArr(j) += arr(i)(j)
        j = j + 1
      }
      maxArea = Math.max(maxHistoryGram(sArr), maxArea)
      i = i + 1
    }
    println(maxArea)
  }

  def maxHistoryGram(arr: Array[Int]): Int = {
    val st = new mutable.Stack[Int]()

    var maxArea = 0
    var area = 0
    var i = 0
    val n = arr.length
    while (i < n) {
      if (st.isEmpty || arr(st.head) < arr(i)) {
        st.push(i)
        i = i + 1
      } else {
        val top = st.pop
        area = arr(top) * (if (st.isEmpty) i else i - st.head - 1)
        maxArea = Math.max(area, maxArea)
      }
    }
    while (st.nonEmpty) {
      val top = st.pop
      area = arr(top) * (if (st.isEmpty) i else i - st.head - 1)
      maxArea = Math.max(area, maxArea)
    }
    maxArea
  }
}
