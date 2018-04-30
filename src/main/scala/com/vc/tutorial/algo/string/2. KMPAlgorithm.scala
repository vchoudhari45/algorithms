package com.vc.tutorial.algo.string

import scala.collection.mutable.ListBuffer

/**
  * Knuth-Morris-Pratt
  * Gives you new element to match with from pattern in case two characters doesn' match
  * https://www.youtube.com/watch?v=GTJr8OvyEVQ&t=492s
  * Time Complexity O(m + n)
  **/
object KMPAlgorithm {

  def main(args: Array[String]): Unit = {
    val pattern = "aa"
    val text = "aaaaa"
    /*val text = (for{
      i <- 1 to 1000000
    }yield{
      "a"
    }).mkString("")*/
    val t = search(pattern, text)
    println(s"Pattern found at (${t.mkString(", ")})")
  }

  /**
    * This function assumes Symbol '$' is not used in the pattern or text
    *
    * @param pattern : String to be search
    * @param text    : String in which pattern will be searched
    * @return List of all the indexes where pattern found in the text
    **/
  def search(pattern: String, text: String): ListBuffer[Int] = {
    val str = pattern + '$' + text
    val n = pattern.length
    val prefixArray = prefixFunction(str)
    println(prefixArray.mkString(""))
    val out = new ListBuffer[Int]()
    prefixArray.indices.foreach(i => {
      if (prefixArray(i) == n) {
        out += (i - 2 * n)
      }
    })
    out
  }


  /**
    *   abra$abracadabra
    *   0001012340101234
    *
    *   aa$aaaaa
    *   01012222
    **/
  private def prefixFunction(str: String): Array[Int] = {
    val prefixArray = new Array[Int](str.length)

    var i = 1
    var j = 0
    val n = prefixArray.length
    prefixArray(i) = 0
    /**
            ll$hello
            01000120
      */
    while(i < n){
      if(str.charAt(i) == str.charAt(j)){
        prefixArray(i) = j + 1
        i += 1
        j += 1
      }else{
        if(j == 0){
          prefixArray(i) = 0
          i += 1
        }else{
          j = prefixArray(j - 1)
        }
      }
    }
    prefixArray
  }

}
