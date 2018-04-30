package com.vc.tutorial.algo.dynamic

/**
  * https://www.youtube.com/watch?v=IFNibRVgFBo
  */
object BurstBalloon {
  def main(args: Array[String]): Unit = {
    val b = Array(3, 1, 5, 8)

    val n = b.length
    /**
      * 0 1 2 3
      * 3 1 5 8
      *
      * 0     1       2      3
      * 0   (3,0) (30,0) (159,0) (167,3)
      * 1         (15,1) (135,2) (159,3)
      * 2                (40,2)  (48,3)
      * 3                        (40,3)
      *
      **/

    val arr = Array.ofDim[(Int, Int)](n, n)
    var i = n
    var j = 0
    var k = 0
    while (i >= 0) {
      j = 0
      k = n - i
      while (k < n) {
        //println(s"$j $k")
        if (j == k) {
          arr(j)(k) = ((if (j - 1 < 0) 1 else b(j - 1)) * b(j) * (if (j + 1 >= n) 1 else b(j + 1)), j)
        } else {
          var max = 0
          (j to k).foreach(l => {
            val v =
              if (l == j) {
                arr(l + 1)(k)._1 + (if (l - 1 < 0) 1 else b(l - 1)) * b(l) * (if (k + 1 >= n) 1 else b(k + 1))
              }
              else if (l == k) {
                arr(j)(l - 1)._1 + (if (j - 1 < 0) 1 else b(j - 1)) * b(l) * (if (l + 1 >= n) 1 else b(l + 1))
              } else {
                arr(j)(l - 1)._1 + arr(l + 1)(k)._1 + (if (j - 1 < 0) 1 else b(j - 1)) * b(l) * (if (k + 1 >= n) 1 else b(k + 1))
              }
            if (max < v) {
              max = v
              arr(j)(k) = (max, l)
            }
          })
        }
        j = j + 1
        k = k + 1
      }
      i = i - 1
    }

    arr.indices.foreach(i => {
      println(arr(i).mkString(" "))
    })

  }
}
