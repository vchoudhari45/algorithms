package com.vc.tutorial.algo.graph

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * https://www.youtube.com/watch?v=lAXZGERcDf4
  * Dijkstra's Algorithm: Single Source Shortest Path algorithm
  *   1. Greedy Algorithm.
  *   2. Works on both directed and undirected graph.
  *   3. Doesn't work if graph has negative weight edge.
  *   4. Similar to Prim's Algorithm for finding minimum spanning tree.
  *   5. Time complexity: O(E log(V))
  * pq will have v elements and extract min, add etc takes log(n) time
  * And we have to perform this operation for each edge so e * log(v)
  *   6. Space complexity: O(E + V)
  */
object DijkstraAlgorithm {

  def main(args: Array[String]): Unit = {
    val g = new Graph(6, isDirected = true)
    //A = 0
    //B = 1
    //C = 2
    //D = 3
    //E = 4
    //F = 5

    //Input
    //AB = 5, BC = 2, CD = 3, DF = 2, EF = 3, AE = 2, AD = 9

    //Output
    //A -> 0
    //B -> 5
    //C -> 7
    //D -> 7
    //E -> 2
    //F -> 5
    g.addEdge(0, 1, 3)
    g.addEdge(0, 3, 1)
    g.addEdge(1, 3, 3)
    g.addEdge(1, 2, 1)
    g.addEdge(2, 5, 4)
    g.addEdge(2, 4, 5)
    g.addEdge(2, 3, 1)
    g.addEdge(3, 4, 6)
    g.addEdge(5, 4, 2)
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

      //This can be optimized by using Priority queue based on binary heap data structure
      val pq = new mutable.HashMap[Int, Int]()

      //Initialize all vertex distance as MAX_VALUE
      (0 until n).foreach(i => {
        pq.put(i, Integer.MAX_VALUE)
      })

      //Take a source vertex and update it's distance to zero
      val sourceVertex = 0
      pq.put(sourceVertex, 0)
      dist.put(sourceVertex, 0)
      parent.put(sourceVertex, 0)

      while (pq.nonEmpty) {
        //Remove minimum value
        val (v, d) = pq.minBy(_._2)
        pq.remove(v)

        dist.put(v, d)
        Option(adj(v)) match {
          case Some(list) => {
            list.foreach(x => {
              pq.get(x.to) match {
                case Some(toWeight) => {
                  val newDist = x.weight + d
                  if (newDist < toWeight) {
                    pq.put(x.to, newDist)
                    parent.put(x.to, v)
                  }
                }
                case None => // already found shortest distance for x.to
              }
            })
          }
          case None => // No members
        }
      }
      dist
    }
  }
}
