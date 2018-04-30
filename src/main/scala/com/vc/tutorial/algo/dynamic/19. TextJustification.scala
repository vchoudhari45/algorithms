package com.vc.tutorial.algo.dynamic

/**
  * https://www.youtube.com/watch?v=RORuwHiblPc
  */
object TextJustification {
  def main(args: Array[String]): Unit = {
    val str = "Vishal Choudhari likes to code"
    val width = 10

    val sArr = str.split(" ")
    val sLen = for {i <- sArr.indices} yield {
      sArr(i).length
    }
    val n = sArr.length
    val arr = Array.ofDim[Int](n, n)
    var i = 0
    var j = 0
    while (i < n) {
      j = i
      while (j < n) {
        //println(s"$i $j")
        var lsum = 0
        (i to j).foreach(k => {
          lsum += sLen(k) + 1
        })
        if (lsum <= width + 1) {
          arr(i)(j) = Math.pow(width + 1 - lsum, 2).toInt
        } else {
          arr(i)(j) = 1000
        }
        j = j + 1
      }
      i = i + 1
    }

    i = 0
    j = 0
    val arrM = new Array[Int](n)
    val arrI = new Array[Int](n)
    while (j < n) {
      if (arr(i)(j) != 1000) {
        arrM(j) = arr(i)(j)
        arrI(j) = j
      } else {
        var min = Int.MaxValue
        arrI(j) = j
        (i until j).foreach(k => {
          val d = arrM(k) + arr(k + 1)(j)
          if (min > d) {
            min = d
            arrI(j) = k + 1
          }
        })
        arrM(j) = min
      }
      i = 0
      j = j + 1
    }
    println(arrM.mkString("\t"))
    println(arrI.mkString("\t"))
  }
}


