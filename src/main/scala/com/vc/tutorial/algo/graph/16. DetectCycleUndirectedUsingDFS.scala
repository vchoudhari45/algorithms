package com.vc.tutorial.algo.graph

import scala.collection.mutable.ListBuffer

/**
  * https://www.youtube.com/watch?v=n_t0a_8H8VY
  */
object DetectCycleUndirectedUsingDFS {

  def main(args: Array[String]): Unit = {
    val g = new Graph(6, false)
    g.addEdge(0, 1)
    g.addEdge(1, 2)
    g.addEdge(2, 3)
    g.addEdge(3, 4)
    g.addEdge(1, 4)
    g.addEdge(0, 5)
    println(g.detectCycle())
  }

  case class Edge(to: Int)

  class Graph(n: Int, isDirected: Boolean) {
    val adj = new Array[ListBuffer[Edge]](n)
    private val visited = Array.fill(n)(false)

    def addEdge(from: Int, to: Int): Unit = {
      Option(adj(from)) match {
        case Some(list) => {
          val e = Edge(to)
          list += e
        }
        case None => {
          val e = Edge(to)
          val list = new ListBuffer[Edge]()
          list += e
          adj(from) = list
        }
      }
    }

    def detectCycle(): Boolean = {
      adj.indices.foreach(i => {
        if (dfsUtil(i)) return true
      })
      false
    }

    private def dfsUtil(n: Int): Boolean = {
      visited(n) = true
      Option(adj(n)) match {
        case Some(list) => {
          list.foreach(j => {
            if (visited(j.to)) return true
            else {
              visited(j.to) = true
            }
          })
          false
        }
        case None => false
      }
    }
  }
}
