package com.vc.tutorial.algo.graph

import scala.collection.mutable.ListBuffer

/**
  * https://www.youtube.com/watch?v=rKQaZuoUR4M
  */
object DetectCycleDirected {

  def main(args: Array[String]): Unit = {
    val g = new Graph(6, true)
    g.addEdge(0, 1)
    g.addEdge(1, 2)
    g.addEdge(0, 2)
    g.addEdge(3, 0)
    g.addEdge(3, 4)
    g.addEdge(4, 5)
    g.addEdge(5, 3)
    println(g.containsCycle())
  }

  case class Edge(to: Int)

  class Graph(n: Int, isDirected: Boolean) {

    private val adj = new Array[ListBuffer[Edge]](n)
    private val w = Array.fill(n)(true)
    private val g = Array.fill(n)(false)
    private val b = Array.fill(n)(false)

    def addEdge(from: Int, to: Int): Unit = {
      Option(adj(from)) match {
        case Some(list) => {
          val e = Edge(to)
          list += e
        }
        case None => {
          val l = new ListBuffer[Edge]()
          val e = Edge(to)
          l += e
          adj(from) = l
        }
      }
    }

    def containsCycle(): Boolean = {
      val n = adj.length
      (0 until n).foreach(i => {
        if (w(i)) {
          if (dfsUtil(i)) return true
        }
      })
      if (b.contains(false)) true else false
    }

    def dfsUtil(n: Int): Boolean = {
      if (g(n)) true
      else {
        if (!b(n)) {
          g(n) = true
          w(n) = false
          Option(adj(n)) match {
            case Some(list) => list.foreach(l => {
              if (dfsUtil(l.to)) return true
            })
            case None =>
          }
          g(n) = false
          b(n) = true
        }
        false
      }
    }
  }
}
