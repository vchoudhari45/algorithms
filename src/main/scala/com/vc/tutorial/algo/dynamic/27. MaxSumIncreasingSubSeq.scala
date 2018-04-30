package com.vc.tutorial.algo.dynamic

import scala.collection.mutable

/**
  * https://www.youtube.com/watch?v=99ssGWhLPUE
  */
object MaxSumIncreasingSubSeq {
  def main(args: Array[String]): Unit = {
    val arr = Array(4, 6, 1, 3, 8, 4, 6)

    val n = arr.length
    val arrMax = Array.fill(n)(0)
    val arrSeq = Array.fill(n)(-1)
    arr.indices.foreach(i => {
      arrMax(i) = arr(i)
    })

    /**
      * 0  1   2  3  4  5  6
      * 4  6   1  3  8  4  6
      *
      * 4  10  1  3  8  4  6
      * -1 0  -1 -1 -1 -1 -1
      * i  j
      *
      * 4  10  1  3  8  4  6
      * -1 0  -1 -1 -1 -1 -1
      * i      j
      *
      * 4  10  1  3  8  4  6
      * -1 0  -1 -1 -1 -1 -1
      * i   j
      *
      **/

    var j = 1
    var i = 0
    while (j < n) {
      i = 0
      while (i < j) {
        //println(s"$i $j")
        if (arr(j) > arr(i)) {
          if (arrMax(j) < arrMax(i) + arr(j)) {
            arrMax(j) = arrMax(i) + arr(j)
            arrSeq(j) = i
          }
        }
        i = i + 1
      }
      j = j + 1
    }

    val maxSum = arrMax.max
    var idx = arrMax.indexOf(maxSum)
    val st = new mutable.Stack[Int]()
    while (idx >= 0) {
      st.push(arr(idx))
      idx = arrSeq(idx)
    }
    println(s"MaxSum: $maxSum")
    println(s"Increasing Seq: ${st.mkString(" -> ")}")
  }
}

