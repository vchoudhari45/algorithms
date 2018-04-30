package com.vc.tutorial.algo.string

import java.util
import java.util.Comparator

/**
  * Idea of suffix array is based on Burrows-Wheeler transform
  * 1. Form all cyclic rotations of text
  * 2. Sort all rotations
  * 3. Last column of array formed is called burrows-wheeler transform
  *
  * Suffix Arrays:
  * All the suffixes of String s in lexicographic order
  *
  * http://www.geeksforgeeks.org/suffix-array-set-2-a-nlognlogn-algorithm/
  * http://www.geeksforgeeks.org/%C2%AD%C2%ADkasais-algorithm-for-construction-of-lcp-array-from-suffix-array/
  *
  * http://www.cs.jhu.edu/~langmea/resources/lecture_notes/suffix_arrays.pdf
  * http://nbviewer.jupyter.org/gist/BenLangmead/6765182
  */
object SuffixArray {

  def main(args: Array[String]): Unit = {
    println(buildSuffixArray("banana".toCharArray).mkString(","))
  }

  def buildSuffixArray(s: Array[Char]): Array[Int] = {
    case class Suffix(idx: Int, var rank: Int, var nextRank: Int)
    def cmp(s1: Suffix, s2: Suffix): Int = {
      val c = s1.rank.compareTo(s2.rank)
      if (c == 0) {
        s1.nextRank.compareTo(s2.nextRank)
      } else {
        c
      }
    }

    val n = s.length
    val suffixes = new Array[Suffix](n)

    s.indices.foreach(i => {
      suffixes(i) = Suffix(
        i,
        s(i) - 'a',
        if (i + 1 < n) s(i + 1) - 'a' else -1
      )
    })

    util.Arrays.sort(suffixes, new Comparator[Suffix] {
      override def compare(o1: Suffix, o2: Suffix): Int = {
        cmp(o1, o2)
      }
    })

    val idxArray = new Array[Int](n)
    for {
      k <- Iterator.iterate(4)(_ * 2) takeWhile (_ <= 2 * n)
    } {
      var rank = 0
      var previousRank = suffixes(0).rank
      suffixes(0).rank = rank
      idxArray(suffixes(0).idx) = 0

      //Assign rank
      for {
        i <- 1 until n
      } {
        if (suffixes(i).rank == previousRank && suffixes(i - 1).nextRank == suffixes(i).nextRank) {
          previousRank = suffixes(i).rank
          suffixes(i).rank = rank
        } else {
          rank = rank + 1
          previousRank = suffixes(i).rank
          suffixes(i).rank = rank
        }
        idxArray(suffixes(i).idx) = i
      }

      //Assign nextRank
      suffixes.indices.foreach(i => {
        val nextIndex = suffixes(i).idx + k / 2
        suffixes(i).nextRank = if (nextIndex < n) suffixes(idxArray(nextIndex)).rank else -1
      })

      util.Arrays.sort(suffixes, new Comparator[Suffix] {
        override def compare(o1: Suffix, o2: Suffix): Int = {
          cmp(o1, o2)
        }
      })
    }
    suffixes.map(_.idx)
  }
}