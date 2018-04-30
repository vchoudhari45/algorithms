package com.vc.tutorial.algo.string

/**
  * Manacher's Algorithm
  * https://leetcode.com/problems/palindromic-substrings/solution/
  **/
object PalindromicSubstring{
  def main(args: Array[String]): Unit = {
    /**
      *  "abba"
      *  "@#a#b#b#a#$"
      *  "012345678910"
      *  "00101410100"
      **/
    val str = "aaa"
    println(manacher(str).mkString(", "))
  }

  def manacher(str: String): Array[Int] = {
    val n = str.length
    val sArr = new Array[Char](2 * n + 3)
    sArr(0) = '@'
    sArr(1) = '#'
    sArr(sArr.length - 1) = '$'

    var i = 2
    str.foreach(x=>{
      sArr(i) = x
      sArr(i + 1) = '#'
      i += 2
    })

    var center = 0
    var right = 0
    val p = new Array[Int](sArr.length)
    var j = 1
    while(j < p.length - 1){
      if(j < right){
        p(j) = Math.min(right - j, p(2 * center - j))
      }
      while(sArr(j + p(j) + 1) == sArr(j - p(j) - 1))
        p(j) += 1
      if(p(j) + j > right){
        right = p(j) + j
        center = j
      }
      j = j + 1
    }
   p
  }
}