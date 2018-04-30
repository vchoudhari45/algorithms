package com.vc.tutorial.algo.dynamic

/**
  * https://www.youtube.com/watch?v=YDf982Lb84o
  */
object NumberOfBinarySearchTree {
  /**
    * 0 1
    * 1 1
    * 2 2
    * 3 5  10, 11, 12
    *
    * 10 <- 1 * 2
    * 11 <- 1 * 1 * 1
    * 12 <- 1 * 2
    *
    *
    * 0 <-  arr(1) * arr(2)
    * 1 <-  arr(1) * arr(1) * arr(1)
    * 2 <-  arr(2) * arr(1)
    *
    * 1
    * 10
    * 11
    * 12
    * 2
    * 10
    * 12
    * 11
    *
    * 3
    * 11
    * 10   12
    *
    *
    * 4
    * 12
    * 10
    * 11
    *
    * 5
    * 12
    * 11
    * 10
    **/
  def main(args: Array[String]): Unit = {
    val n = 6
    if (n == 0) println(0)
    else {
      val arr = new Array[Int](n)
      (0 until n).foreach(i => {
        if (i == 0) arr(0) = 1
        else if (i == 1) arr(1) = 1
        else {
          var sum = 0
          (0 until i).foreach(j => {
            sum += arr(j) * (if (i - j - 1 < 0) 1 else arr(i - j - 1))
          })
          arr(i) = sum
        }
      })
      (1 until arr.length).foreach(i => println(s"$i => ${arr(i)}"))
    }
  }
}
