package com.vc.tutorial.algo.graph

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Topological Sorting
  */
object TopologicalSorting {

  def main(args: Array[String]): Unit = {
    val g = new Graph(6)
    g.addEdge(0, 1, 3, isDirected = true)
    g.addEdge(0, 3, 1, isDirected = true)
    g.addEdge(1, 3, 3, isDirected = true)
    g.addEdge(1, 2, 1, isDirected = true)
    g.addEdge(2, 5, 4, isDirected = true)
    g.addEdge(2, 4, 5, isDirected = true)
    g.addEdge(2, 3, 1, isDirected = true)
    g.addEdge(3, 4, 6, isDirected = true)
    g.addEdge(5, 4, 2, isDirected = true)
    g.topologicalSorting()
  }

  case class Edge(to: Int, weight: Int)

  class Graph(n: Int) {
    val adj = new Array[ListBuffer[Edge]](n)

    def addEdge(from: Int, to: Int, weight: Int, isDirected: Boolean): Unit = {
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

    def topologicalSorting(): Unit = {
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
          val ele = st.pop
          Option(adj(ele)) match {
            case Some(list) => list.foreach(x => {
              if (!visited(x.to)) {
                visited(x.to) = true
                st.push(x.to)
              }
            })
            case None => //No Members
          }
          print(ele + " ")
        }
      }
    }
  }
}
