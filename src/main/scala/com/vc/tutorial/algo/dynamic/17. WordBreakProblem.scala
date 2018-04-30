package com.vc.tutorial.algo.dynamic

import scala.collection.mutable

/**
  * https://www.youtube.com/watch?v=WepWFGxiwRs
  */
object WordBreakProblem {
  def main(args: Array[String]): Unit = {
    val str = "Iamace"
    val n = str.length
    val dictionary = new mutable.HashSet[String]()
    dictionary.add("I")
    dictionary.add("a")
    dictionary.add("am")
    dictionary.add("ace")

    /**
      *
      * I a m a c e
      * 0 1 2 3 4 5
      * 0 T T T T
      * 1   T T T
      * 2     F F F
      * 3       T F T
      * 4         F F
      * 5           F
      *
      **/
    val arr = Array.ofDim[(Boolean, Int)](n, n)
    var i = n - 1
    var j = 0
    var k = 0
    while (i >= 0) {
      j = 0
      k = n - i - 1
      while (k < n) {
        //println(s"$j $k")
        val s = str.substring(j, k + 1)
        if (dictionary.contains(s)) {
          arr(j)(k) = (true, -1)
        } else {
          if (j != k) {
            var flag = false
            import scala.util.control.Breaks._
            breakable {
              (j until k).foreach(l => {
                if (arr(j)(l)._1 && arr(l + 1)(k)._1) {
                  arr(j)(k) = (true, l)
                  flag = true
                  break
                }
              })
            }
            if (!flag) {
              arr(j)(k) = (false, -1)
            }
          } else {
            arr(j)(k) = (false, -1)
          }
        }
        j = j + 1
        k = k + 1
      }
      i = i - 1
    }

    arr.indices.foreach(i => {
      arr(i).indices.foreach(j => {
        Option(arr(i)(j)) match {
          case Some(value) => print(value + "\t")
          case None => print("( , )\t\t")
        }
      })
      println()
    })
  }
}
