package com.vc.tutorial.algo.graph

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
  * https://www.youtube.com/watch?v=n_t0a_8H8VY
  */
object DetectCycleUndirected {

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
    private val parent = new Array[Int](n)

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
    parent.indices.foreach(i => {
      parent(i) = i
    })

    def detectCycle(): Boolean = {
      adj.indices.foreach(i => {
        Option(adj(i)) match {
          case Some(list) => list.foreach(j => {
            val parentX = find(j.to)
            val parentY = find(i)
            if (parentX == parentY) return true
            else union(parentX, parentY)
          })
          case None =>
        }
      })
      false
    }

    @tailrec private def find(x: Int): Int = {
      if (x == parent(x)) return x
      find(parent(x))
    }

    private def union(x: Int, y: Int): Unit = {
      parent(x) = y
    }
  }
}
