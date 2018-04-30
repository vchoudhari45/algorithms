package com.vc.tutorial.algo.dynamic

/**
  * https://www.youtube.com/watch?v=3hcaVyX00_4
  */
object EggDroppingProblem {
  def main(args: Array[String]): Unit = {
    val floor = 6
    val eggs = 2

    /**
      * 0 1 2 3 4 5 6
      * 0 0 0 0 0 0 0 0
      * 1 0 1 2 3 4 5 6
      * 2 0 1 2 2 3 3 3
      **/
    val arr = Array.ofDim[Int](eggs + 1, floor + 1)
    (1 to eggs).foreach(e => {
      (1 to floor).foreach(f => {
        if (e == 1) {
          arr(e)(f) = f
        } else if (f - e >= 0) {
          /**
            *
            * 1 + Math.max(
            **/
          var min = Integer.MAX_VALUE
          (1 to f).foreach(k => {
            min = Math.min(1 +
              Math.max(
                arr(e - 1)(k - 1),
                arr(e)(f - k)
              ),
              min
            )
          })
          arr(e)(f) = min
        } else {
          arr(e)(f) = arr(e - 1)(f)
        }
      })
    })
    arr.indices.foreach(i => {
      println(arr(i).mkString(", "))
    })

  }
}
