package com.vc.tutorial.algo.dynamic

/**
  * https://www.youtube.com/watch?v=yCQN096CwWM
  */
object MaxRectangleSumMatrix {
  def main(args: Array[String]): Unit = {
    val arr = Array.ofDim[Int](4, 5)

    arr(0)(0) = 2
    arr(0)(1) = 1
    arr(0)(2) = -3
    arr(0)(3) = -4
    arr(0)(4) = 5

    arr(1)(0) = 0
    arr(1)(1) = 6
    arr(1)(2) = 3
    arr(1)(3) = 4
    arr(1)(4) = 1

    arr(2)(0) = 2
    arr(2)(1) = -2
    arr(2)(2) = -1
    arr(2)(3) = 4
    arr(2)(4) = -5

    arr(3)(0) = -3
    arr(3)(1) = 3
    arr(3)(2) = 1
    arr(3)(3) = 0
    arr(3)(4) = 3

    val ro = arr.length
    val co = arr(0).length

    var mRi = 0
    var mLe = 0
    var mR1 = 0
    var mR2 = 0
    var maxSum = Integer.MIN_VALUE

    var ri = 0
    var le = 0
    val sArr = Array.fill(ro)(0)
    while (le < co) {
      ri = le
      while (ri < co) {
        sArr.indices.foreach(i => {
          sArr(i) += arr(i)(ri)
        })
        val (r1, r2, sum) = kadane(sArr)
        if (sum > maxSum) {
          maxSum = sum
          mLe = le
          mRi = ri
          mR1 = r1
          mR2 = r2
        }
        ri = ri + 1
      }
      sArr.indices.foreach(i => {
        sArr(i) = 0
      })
      le = le + 1
    }
    println(s"$mLe $mRi $mR1 $mR2 $maxSum")

  }

  def kadane(arr: Array[Int]): (Int, Int, Int) = {
    var maxSum = Integer.MIN_VALUE
    var mFrom = 0
    var mTo = 0

    var sum = 0
    var from = 0
    arr.indices.foreach(i => {
      if (sum + arr(i) < 0) {
        sum = 0
        from = from + 1
      } else {
        sum += arr(i)
      }
      if (sum > maxSum) {
        if (i == 0) {
          maxSum = arr(0)
          mFrom = 0
        } else {
          maxSum = sum
          mFrom = from
          mTo = i
        }
      }
    })
    (mFrom, mTo, maxSum)
  }
}
