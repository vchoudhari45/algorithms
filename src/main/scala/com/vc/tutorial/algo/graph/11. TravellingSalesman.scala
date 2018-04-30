package com.vc.tutorial.algo.graph

import scala.collection.mutable

/**
  * https://www.youtube.com/watch?v=2kREIkF9UAs
  */
object TravellingSalesman {

  def main(args: Array[String]): Unit = {
    val g = new Graph(4, true)
    g.addEdge(0, 0, 0)
    g.addEdge(0, 1, 1)
    g.addEdge(0, 2, 15)
    g.addEdge(0, 3, 6)

    g.addEdge(1, 0, 2)
    g.addEdge(1, 1, 0)
    g.addEdge(1, 2, 7)
    g.addEdge(1, 3, 3)

    g.addEdge(2, 0, 9)
    g.addEdge(2, 1, 6)
    g.addEdge(2, 2, 0)
    g.addEdge(2, 3, 12)

    g.addEdge(3, 0, 10)
    g.addEdge(3, 1, 4)
    g.addEdge(3, 2, 8)
    g.addEdge(3, 3, 0)

    g.ts()
  }

  class Graph(n: Int, isDirected: Boolean) {
    val adj = new Array[Array[Int]](n)
    adj.indices.foreach(i => adj(i) = new Array[Int](n))

    def addEdge(from: Int, to: Int, weight: Int): Unit = {
      adj(from)(to) = weight
    }

    def ts(): Unit = {
      val source = 0
      val parent = new mutable.HashMap[(Int, IndexedSeq[Int]), (Int, Int)]()
      var minDistance = Integer.MAX_VALUE
      (0 until n).foreach(i => {
        val itr = (1 until n).combinations(i)
        while (itr.hasNext) {
          val set = itr.next
          (1 until n).filter(x => !set.contains(x)).foreach(k => {
            minDistance = Integer.MAX_VALUE
            set.foreach(x => {
              val dist = adj(x)(k) + parent((x, set.filter(y => y != x)))._1
              if (minDistance > dist) {
                parent.put((k, set), (dist, x))
                minDistance = dist
              }
            })
            if (set.nonEmpty) {
              //println(s"Minimum distance from $source to $k via (${set.mkString(", ")}) is $minDistance")
            }
            else {
              parent.put((k, IndexedSeq()), (adj(source)(k), -1))
              // println(s"Minimum distance from $source to $k via () is ${adj(source)(k)}")
            }
          })
        }
      })

      minDistance = Integer.MAX_VALUE
      val set = 1 until n
      var path = 0
      (1 until n).foreach(i => {
        val setFiltered = set.filter(x => x != i)
        val (d, p) = parent(i, setFiltered)
        val dist = d + adj(i)(source)
        if (minDistance > dist) {
          minDistance = Math.min(minDistance, dist)
          path = i
        }
      })

      print(source + " <- ")
      var setISeq: IndexedSeq[Int] = set
      while (path != -1) {
        print(path + " <- ")
        setISeq = setISeq.filter(x => x != path)
        path = parent(path, setISeq)._2
      }
      println(source)
      println(minDistance)
    }

  }
}
