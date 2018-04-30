package com.vc.tutorial.algo.graph

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * https://www.youtube.com/watch?v=-mOEd_3gTK0
  * Bellman Ford's Algorithm: Single Source Shortest Path Algorithm
  *   1. Greedy Algorithm.
  *   2. Works on directed graph.
  *   3. Works on the directed graph with non-negative as well as negative weight edge.
  *   4. Doesn't work on undirected graph if any of the edge has negative weight, because
  * negative weight undirected edge will be counted as loop.
  *   5. Doesn't work on graph that contains negative weight cycle
  * Negative weight cycle is a cycle whose overall weight is negative.
  *   6. Time complexity: O(V * E) We go through all the edges V - 1 times and one more time to see if there is any negatie weight cycle
  *   7. Space complexity: O(V)
  */
object BellmanFordAlgorithm {

  def main(args: Array[String]): Unit = {

    //    This is an example of negative weight cycle
    //    val g = new Graph(4, true)
    //    g.addEdge(0, 1, 1)
    //    g.addEdge(1, 2, 3)
    //    g.addEdge(2, 3, 2)
    //    g.addEdge(3, 1, -6)
    //    println(g.shortestPath())

    val g = new Graph(5, true)
    g.addEdge(3, 4, 2)
    g.addEdge(4, 3, 1)
    g.addEdge(2, 4, 4)
    g.addEdge(0, 2, 5)
    g.addEdge(1, 2, -3)
    g.addEdge(0, 3, 8)
    g.addEdge(0, 1, 4)
    println(g.shortestPath())
  }

  case class Edge(to: Int, weight: Int)

  class Graph(n: Int, isDirected: Boolean) {
    val adj = new Array[ListBuffer[Edge]](n)

    def addEdge(from: Int, to: Int, weight: Int): Unit = {
      Option(adj(from)) match {
        case Some(list) => {
          val e = Edge(to, weight)
          list += e
        }
        case None => {
          val l = new ListBuffer[Edge]()
          val e = Edge(to, weight)
          l += e
          adj(from) = l
        }
      }
      if (!isDirected) {
        Option(adj(to)) match {
          case Some(list) => {
            val e = Edge(from, weight)
            list += e
          }
          case None => {
            val l = new ListBuffer[Edge]()
            val e = Edge(from, weight)
            l += e
            adj(to) = l
          }
        }
      }
    }

    def shortestPath(): mutable.HashMap[Int, Int] = {
      val dist = new mutable.HashMap[Int, Int]()
      val parent = new mutable.HashMap[Int, Int]()

      (0 until n).foreach(i => {
        dist.put(i, 100000)
      })

      dist.put(0, 0)
      (1 until n).foreach(i => {
        (0 until n).foreach(j => {
          Option(adj(j)) match {
            case Some(list) => list.foreach(k => {
              if (dist(k.to) > dist(j) + k.weight) {
                dist(k.to) = dist(j) + k.weight
                parent.put(k.to, j)
              }
            })
            case None =>
          }
        })
      })

      //Apply relaxing formula once more to see if there is negative weight cycle
      (0 until n).foreach(j => {
        Option(adj(j)) match {
          case Some(list) => list.foreach(k => {
            if (dist(k.to) > dist(j) + k.weight) {
              throw new Exception("Negative weight cycle exist, can't find shortest path for this graph")
            }
          })
          case None =>
        }
      })
      dist
    }
  }
}
