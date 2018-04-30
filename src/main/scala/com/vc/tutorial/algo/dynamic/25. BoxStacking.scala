package com.vc.tutorial.algo.dynamic

/**
  * https://www.youtube.com/watch?v=9mod_xRB-O0
  */
object BoxStacking {
  def main(args: Array[String]): Unit = {
    val b1 = Array(1, 2, 4)
    val b2 = Array(3, 2, 5)

    val total = (b1.permutations ++ b2.permutations).toArray.sortBy(x => x(0) * x(1))
    total.indices.foreach(i => {
      println(total(i).mkString(" "))
    })

    val n = total.length
    val arr = new Array[Int](n)
    arr.indices.foreach(i => arr(i) = total(i)(2))
    val idx = Array.fill(n)(-1)

    var i = 0
    var j = 1
    while (j < n) {
      i = 0
      while (i < j) {
        //println(s"$i $j")
        if (canTop(i, j)) {
          if (arr(j) < arr(i) + total(j)(2)) {
            arr(j) = Math.max(arr(j), arr(i) + total(j)(2))
            idx(j) = i
          }
        }
        i = i + 1
      }
      j = j + 1
    }

    def canTop(i: Int, j: Int): Boolean = total(i)(0) < total(j)(0) && total(i)(1) < total(j)(1)

    println(arr.mkString(" "))
    println(idx.mkString(" "))
  }
}