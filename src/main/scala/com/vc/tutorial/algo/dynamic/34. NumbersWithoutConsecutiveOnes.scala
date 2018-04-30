package com.vc.tutorial.algo.dynamic

/**
  * https://www.youtube.com/watch?v=a9-NtLIs1Kk
  */
object NumbersWithoutConsecutiveOnes {
  def main(args: Array[String]): Unit = {
    val n = 4

    var oldSum = 0
    var newSum = 1
    //println(0)
    (0 to n).foreach(i => {
      //println(newSum)
      val temp = newSum
      newSum = oldSum + newSum
      oldSum = temp
    })
    println(newSum)
  }
}
