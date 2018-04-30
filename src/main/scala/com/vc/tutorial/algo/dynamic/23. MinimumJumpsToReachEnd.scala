package com.vc.tutorial.algo.dynamic

/**
  * https://www.youtube.com/watch?v=cETfFsSTGJI
  */
object MinimumJumpsToReachEnd {
  def main(args: Array[String]): Unit = {
    val arr = Array(2, 3, 1, 1, 2, 4, 2, 0, 1, 1)
    /**
      *
      * 0 1 2 3 4 5 6 7 8 9
      * 2 3 1 1 2 4 2 0 1 1
      *
      * 0 1 0 0 0 0 0 0 0 0   Jumps(Min)
      * 0 0 0 0 0 0 0 0 0 0   From
      * i j
      *
      * 0 1 2 0 0 0 0 0 0 0   Jumps(Min)
      * 0 0 0 0 0 0 0 0 0 0   From
      * i   j
      *
      * 0 1 2 3 0 0 0 0 0 0   Jumps(Min)
      * 0 0 0 1 0 0 0 0 0 0   From
      * i   j
      *
      * 0 1 2 3 0 0 0 0 0 0   Jumps(Min)
      * 0 0 0 1 0 0 0 0 0 0   From
      * i j
      **/

    val n = arr.length
    val jump = Array.fill(n)(Int.MaxValue)
    val from = Array.fill(n)(-1)

    var i = 0
    var j = 1
    jump(0) = 0
    from(0) = 0
    while (j < n) {
      i = 0
      while (i < j) {
        if (j <= arr(i) + i) {
          if (jump(j) > jump(i) + 1) {
            jump(j) = jump(i) + 1
            from(j) = i
          }
          i = i + 1
        } else {
          i = i + 1
        }
      }
      j = j + 1
    }
    println(jump.mkString(" "))
    println(from.mkString(" "))
    println(from.indices.mkString(" "))
  }
}
