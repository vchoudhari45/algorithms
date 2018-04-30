package com.vc.tutorial.algo.graph

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * https://www.youtube.com/watch?v=fAuF0EuZVCk
  * Prim's Algorithm: Minimum spanning tree
  *   1. Greedy Algorithm
  *   2. Works on only un directed graph
  *   3. Time complexity: O(E * log(V))
  *   4. Space complexity: O(E + V)
  */
object PrimAlgorithm {

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
      val pq = new mutable.HashMap[Int, Int]()
      val edges = new mutable.HashMap[Int, Edge]()
      val g = new Graph(n, isDirected)

      (0 until n).foreach(i => {
        pq.put(i, Integer.MAX_VALUE)
      })


      pq.put(0, 0)
      while (pq.nonEmpty) {
        val (v, d) = pq.minBy(_._2)
        pq.remove(v)

        edges.get(v) match {
          case Some(e) => Option(g.adj(v)) match {
            case Some(list) => {
              list += e
            }
            case None => {
              val l = new ListBuffer[Edge]
              l += e
              g.adj(v) = l
            }
          }
          case None => // No edge found
        }

        Option(adj(v)) match {
          case Some(list) => list.foreach(x => {
            if (pq.contains(x.to) && pq(x.to) > x.weight) {
              pq.put(x.to, x.weight)
              edges.put(x.to, Edge(v, x.weight))
            }
          })
          case None => // No members
        }
      }
      g
    }
  }

}
