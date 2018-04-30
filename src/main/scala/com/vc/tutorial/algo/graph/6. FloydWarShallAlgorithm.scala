package com.vc.tutorial.algo.graph

/**
  * https://www.youtube.com/watch?v=LwJdNfdLF9s
  * FloydWarShall's Algorithm: All pair shortest path algorithm
  * 1. Dynamic programing Algorithm
  * 2. Works on both directed and un directed graph
  * 3. Works on graph with negative weight edge
  * 4. Time complexity: O(V3)
  * 5. Space complexity: O(V2)
  */
object FloydWarShallAlgorithm {

  def main(args: Array[String]): Unit = {
    val g = new Graph(4, true)
    g.addEdge(0, 1, 3)
    g.addEdge(1, 2, -2)
    g.addEdge(0, 2, 6)
    g.addEdge(0, 3, 15)
    g.addEdge(3, 0, 1)
    g.addEdge(2, 3, 2)
    val out = g.allPairShortestPath()

    println("Distance:")
    out._1.indices.foreach(i => {
      println(out._1(i).mkString(", "))
    })

    println("\nPaths:")
    out._2.indices.foreach(i => {
      println(out._2(i).mkString(", "))
    })
  }

  class Graph(n: Int, isDirected: Boolean) {
    val dist = new Array[Array[Int]](n)
    val path = new Array[Array[Int]](n)

    dist.indices.foreach(i => {
      dist(i) = new Array[Int](n)
      path(i) = new Array[Int](n)
      dist(i).indices.foreach(j => {
        if (i == j) {
          dist(i)(j) = 0
          path(i)(i) = -1
        }
        else {
          dist(i)(j) = 10000
        }
      })
    })

    def addEdge(from: Int, to: Int, weight: Int): Unit = {
      dist(from)(to) = weight
      path(from)(to) = from
      if (!isDirected) {
        dist(to)(from) = weight
        path(to)(from) = to
      }
    }

    def allPairShortestPath(): (Array[Array[Int]], Array[Array[Int]]) = {
      (0 until n).foreach(k => {
        (0 until n).foreach(i => {
          (0 until n).foreach(j => {
            if (dist(i)(j) > dist(i)(k) + dist(k)(j)) {
              dist(i)(j) = dist(i)(k) + dist(k)(j)
              path(i)(j) = path(k)(j)
            }
          })
        })
      })

      //Detecting negative weight cycle
      (0 until n).foreach(i => {
        if (dist(i)(i) < 0) throw new Exception("Negative weight cycle exists in the graph")
      })

      (dist, path)
    }
  }
}
