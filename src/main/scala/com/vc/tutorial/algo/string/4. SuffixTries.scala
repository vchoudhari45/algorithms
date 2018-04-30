package com.vc.tutorial.algo.string

import java.util.Scanner

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * SuffixTree
  * A Suffix Tree for a given text is a compressed trie for all suffixes of the given text
  *
  * Cons:
  * ~20 * |Text| memory footprint causes it to go out of memory
  **/

/**
  * SuffixTries(Approach 1)
  * Let T be the search string in which you need to find patterns
  * Let P1, P2, P3, ....Pn are the pattern of different length
  * If we want to match each of this pattern individually into the search string T, then it will take n iteration through the string
  *
  * we can combine patterns into tree like structure called(Suffix Tries),
  * So number of iteration through search string T reduces to just one after you have build the suffix tries
  *
  * Pros:
  * Brute-force takes O(|Text|*|Patterns|)
  * TrieMatching takes O(|Text|*|LongestPattern|)
  * TriesConstruction takes O(|Patterns|)
  *
  * Cons:
  * #edges = O(|Total Length of Patterns|) and memory goes out for very large number of patterns
  */

/**
  * SuffixTries(Approach 2)
  * Let T be the search string in which you need to find pattens
  * Let P1, P2, P3, .... Pn are the patterns of different length
  *
  * Instead of packing the patterns into the tree like structure, we can
  *  1. Generates all the suffixes of search string T
  *  2. Pack them into Tries
  *  3. For each pattern checks if it can be spelled out from root to downward in suffix Trie
  *
  * SuffixTree Leaf should contain additional information(Index at which suffix start in search string).
  * For e.g. PANAMABANANAS
  * Suffixes are PANAMABANANAS, ANAMABANANAS, NAMABANANAS .... etc
  * Information about PANAMABANANAS starts at 0 should be present in the leaf S in the suffixTree
  * Information about ANAMABANANAS starts at 1 should be present in the leaf S in the suffixTree
  * Information about NAMABANANAS starts at 2 should be present in the leaf S in the suffixTree ...etc
  *
  * Cons: Memory goes out for very large search string
  **/

/**
  * Below is implementation of Approach 1 of SuffixTries
  **/
case class Node(child: Array[Node], var isLeaf: Boolean)
object SuffixTries {

  private val root = Node(new Array[Node](26), isLeaf = false)
  def add(str: String): Unit = {
    var current = root
    (0 until str.length).foreach(i => {
      val c = str.charAt(i) - 'A'
      if (current.child(c) == null) {
        current.child(c) = Node(new Array[Node](26), isLeaf = false)
        current = current.child(c)
      } else {
        current = current.child(c)
      }
    })
    current.isLeaf = true
  }

  /**
    * @return Array of pattern and index at which pattern found in the search string
    **/
  def search(str: String, numberOfPatterns: Int): mutable.HashMap[String, mutable.ListBuffer[Int]] = {
    val out = new mutable.HashMap[String, ListBuffer[Int]]
    str.toCharArray.indices.foreach(i => {
      var charCounter = i
      var current = root
      val pattern = StringBuilder.newBuilder
      while (charCounter < str.length) {
        val node = current.child(str(charCounter) - 'A')
        pattern.append(str(charCounter))
        if (node != null) {
          current = node
          charCounter += 1
          if (current.isLeaf) {
            out.get(pattern.toString()) match {
              case Some(value) => value += i
              case None => {
                val t = new ListBuffer[Int]()
                t += i
                out.put(pattern.toString(), t)
              }
            }
          }
        } else {
          charCounter = str.length
        }
      }
    })
    out
  }

  /**
    * Sample Input
    * 4
    * AB
    * ABC
    * DF
    * EI
    * ABCABDFABABC
    **/
  def main(args: Array[String]): Unit = {
    val s = new Scanner(System.in)
    val n = s.next.toInt
    (0 until n).foreach(i => {
      add(s.next)
    })
    val searchString = s.next
    val out = search(searchString, n)
    out.foreach(x => {
      println(s"${x._1} => (${x._2.length})")
    })
  }
}
