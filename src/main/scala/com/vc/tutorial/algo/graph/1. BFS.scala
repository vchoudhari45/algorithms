package com.vc.tutorial.algo.graph

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * BFS: Breadth First Search
  */
object BFS {

  def main(args: Array[String]): Unit = {
    val g = new Graph(6, isDirected = true)
    g.addEdge(0, 1, 3)
    g.addEdge(0, 3, 1)
    g.addEdge(1, 3, 3)
    g.addEdge(1, 2, 1)
    g.addEdge(2, 5, 4)
    g.addEdge(2, 4, 5)
    g.addEdge(2, 3, 1)
    g.addEdge(3, 4, 6)
    g.addEdge(5, 4, 2)
    g.bfs()
  }

  case class Edge(to: Int, weight: Int)

  class Graph(n: Int, isDirected: Boolean) {
    val adj = new Array[ListBuffer[Edge]](n)

    def addEdge(from: Int, to: Int, weight: Int): Unit = {
      Option(adj(from)) match {
        case Some(list) => {
          val t = Edge(to, weight)
          list.+=(t)
        }
        case None => {
          val t = Edge(to, weight)
          val l = new ListBuffer[Edge]
          l += t
          adj(from) = l
        }
      }

      if (!isDirected) {
        Option(adj(to)) match {
          case Some(list) => {
            val t = Edge(from, weight)
            list.+=(t)
          }
          case None => {
            val t = Edge(from, weight)
            val l = new ListBuffer[Edge]
            l += t
            adj(to) = l
          }
        }
      }
    }

    def bfs(): Unit = {
      val visited = Array.fill(n)(false)
      val q = new mutable.Queue[Int]()
      (0 until n).foreach(i => {
        if (!visited(i)) {
          bfsUtil(i)
        }
      })

      def bfsUtil(j: Int): Unit = {
        visited(j) = true
        q.enqueue(j)
        while (q.nonEmpty) {
          val k = q.dequeue()
          print(k + " ")
          Option(adj(k)) match {
            case Some(list) => list.foreach(x => {
              if (!visited(x.to)) {
                visited(x.to) = true
                q.enqueue(x.to)
              }
            })
            case None => // No elements
          }
        }
      }
    }
  }
}
