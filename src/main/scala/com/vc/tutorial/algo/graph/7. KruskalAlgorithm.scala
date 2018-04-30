package com.vc.tutorial.algo.graph

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
  * https://www.youtube.com/watch?v=fAuF0EuZVCk
  * Kruskal's Algorithm: Minimum spanning tree
  * It is sub graph such that all the vertices are connected to each other and then there are n - 1 edges in a sub graph
  * Minimum spanning tree is a tree such that sum of the weight of edges is minimum
  *   1. Greedy Algorithm
  *   2. Works on only un directed graph
  *   3. Times complexity: O(E * log(E) + E)
  *   4. Space complexity: O(E + V)
  */
object KruskalAlgorithm {

  def main(args: Array[String]): Unit = {
    val g = new Graph(6, false)
    //0 A
    //1 B
    //2 C
    //3 D
    //4 E
    //5 F


    //0 3 1 AD
    //1 2 1 BC
    //2 3 1 CD
    //2 5 4 CE
    //4 5 2 EF

    //AD 1
    //DE 6
    //AB 3
    //BD 3
    //CD 1
    //CE 5
    //EF 2
    //BC 1
    //CF 4
    g.addEdge(0, 3, 1)
    g.addEdge(3, 4, 6)
    g.addEdge(0, 2, 3)
    g.addEdge(1, 3, 3)
    g.addEdge(2, 3, 1)
    g.addEdge(2, 4, 5)
    g.addEdge(4, 5, 2)
    g.addEdge(1, 2, 1)
    g.addEdge(2, 5, 4)
    val g1 = g.getMST
    g1.adj.indices.foreach(i => {
      Option(g1.adj(i)) match {
        case Some(list) => list.foreach(j => {
          println(i + " " + j.to + " " + j.weight)
        })
        case None =>
      }
    })
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

    def getMST: Graph = {
      val e = new ListBuffer[(Int, Int, Int)]()

      val p = new Array[Int](n)
      p.indices.foreach(i => p(i) = i)

      def isCyclic(x: Int, y: Int): Boolean = {
        val xParent = find(x)
        val yParent = find(y)
        if (xParent == yParent) {
          true
        }
        else {
          union(xParent, yParent)
          false
        }
      }

      def union(x: Int, y: Int): Unit = {
        p(y) = x
      }

      @tailrec def find(x: Int): Int = {
        if (x == p(x)) x
        else find(p(x))
      }

      (0 until n).foreach(i => {
        Option(adj(i)) match {
          case Some(list) => {
            list.foreach(j => {
              e.+=((i, j.to, j.weight))
            })
          }
          case None =>
        }
      })
      val se = e.sortBy(_._3)
      val g = new Graph(n, false)
      var count = 0
      se.foreach(x => {
        if (!isCyclic(x._1, x._2) && count != n - 1) {
          count = count + 1
          Option(g.adj(x._1)) match {
            case Some(list) => {
              list += Edge(x._2, x._3)
            }
            case None => {
              val list = new ListBuffer[Edge]()
              list += Edge(x._2, x._3)
              g.adj(x._1) = list
            }
          }
        }
      })
      g
    }
  }
}
