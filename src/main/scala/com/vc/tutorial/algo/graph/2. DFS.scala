package com.vc.tutorial.algo.graph

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * DFS: Depth First Search
  */
object DFS {

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
    g.dfs()
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

    def dfs(): Unit = {
      val visited = Array.fill(n)(false)
      val st = new mutable.Stack[Int]()
      (0 until n).foreach(i => {
        if (!visited(i)) {
          dfsUtil(i)
        }
      })

      def dfsUtil(j: Int): Unit = {
        visited(j) = true
        st.push(j)
        while (st.nonEmpty) {
          val k = st.pop()
          print(k + " ")
          Option(adj(k)) match {
            case Some(list) => list.foreach(x => {
              if (!visited(x.to)) {
                visited(x.to) = true
                st.push(x.to)
              }
            })
            case None => // No members
          }
        }
      }
    }
  }
}


