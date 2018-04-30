package com.vc.tutorial.algo.string

import scala.collection.mutable.ListBuffer

/**
  * Rabin Karp Algorithm
  * Based on rolling hash
  * https://www.youtube.com/watch?v=H4VrKHVG5qI&t=608s
  */
object RabinKarp {

  private val PRIME_FACTOR = 101
  def main(args: Array[String]): Unit = {
    val text = (for{
       i <- 1 to 1000000
     }yield{
       "a"
     }).mkString("")
    val pattern = "aa"
    println(s"Pattern found at (${search(text, pattern).mkString(", ")})")
  }

  def search(text:String, pattern:String): ListBuffer[Int] = {
    val output = new ListBuffer[Int]()

    val patternLength = pattern.length
    val patternHash = hash(pattern)
    var textOldHash = hash(text.substring(0, patternLength))

    if(patternHash==textOldHash){
      if(checkByChar(0)) output += 0
    }

    (patternLength until text.length).foreach(i=>{
      textOldHash = rollingHash(textOldHash, text.charAt(i), text.charAt(i - patternLength), patternLength)
      if(textOldHash==patternHash){
        if(checkByChar(i - patternLength + 1)) output += (i - patternLength + 1)
      }
    })

    def checkByChar(i:Int): Boolean = {
      var j = 0
      while(j < patternLength){
        if(text.charAt(i + j) != pattern.charAt(j)) return false
        j = j + 1
      }
      true
    }
    output
  }

  private def hash(str:String): Int = {
    var output = 0
    str.indices.foreach(i=>{
      output += str.charAt(i) * Math.pow(PRIME_FACTOR, i).toInt
    })
    output
  }

  private def rollingHash(oldHash:Int, newChar:Char, oldChar:Char, patternLength:Int): Int = {
    ((oldHash - oldChar) / PRIME_FACTOR) + (newChar * Math.pow(PRIME_FACTOR, patternLength - 1).toInt)
  }
}
