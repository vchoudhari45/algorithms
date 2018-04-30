package com.vc.tutorial.algo.graph

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * https://www.youtube.com/watch?v=RpgcYiky7uw
  */
object KosarajuAlgorithm {

  def main(args: Array[String]): Unit = {
    val g = new Graph(11, true)
    //A 0
    //B 1
    //C 2
    //D 3
    //E 4
    //F 5
    //G 6
    //H 7
    //I 8
    //J 9
    //K 10

    //CA 20
    //AB 01
    //BC 12
    //BD 13
    //DE 34
    //EF 45
    //FD 53
    //GF 65
    //HI 78
    //IJ 89
    //JG 96
    //GH 67
    //JK 9 10

    g.addEdge(2, 0, 0)
    g.addEdge(0, 1, 0)
    g.addEdge(1, 2, 0)
    g.addEdge(1, 3, 0)
    g.addEdge(3, 4, 0)
    g.addEdge(4, 5, 0)
    g.addEdge(5, 3, 0)
    g.addEdge(6, 5, 0)
    g.addEdge(7, 8, 0)
    g.addEdge(8, 9, 0)
    g.addEdge(9, 6, 0)
    g.addEdge(6, 7, 0)
    g.addEdge(9, 10, 0)
    g.stronglyConnectedComponent()
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

    def stronglyConnectedComponent(): Unit = {
      val visited = Array.fill(n)(false)
      val st = new mutable.Stack[Int]()

      (1 until n).foreach(i => {
        if (!visited(i)) dfsUtil(i, visited, st, adj)
      })

      visited.indices.foreach(i => visited(i) = false)

      //Reverse the graph
      val adj1 = new Array[ListBuffer[Edge]](n)
      adj.indices.foreach(i => {
        Option(adj(i)) match {
          case Some(list) => {
            list.foreach(x => {
              Option(adj1(x.to)) match {
                case Some(list1) => list1 += Edge(i, x.weight)
                case None => {
                  val l = new ListBuffer[Edge]()
                  l += Edge(i, x.weight)
                  adj1(x.to) = l
                }
              }
            })
          }
          case None => //No members
        }
      })

      while (st.nonEmpty) {
        val e = st.pop
        if (!visited(e)) {
          val s = new mutable.Stack[Int]()
          dfsUtil(e, visited, s, adj1)
          println(s.mkString("  "))
          s.clear
        }
      }
    }

    def dfsUtil(k: Int, visited: Array[Boolean], st: mutable.Stack[Int], adjacency: Array[ListBuffer[Edge]]): Unit = {
      visited(k) = true
      Option(adjacency(k)) match {
        case Some(list) => {
          list.foreach(x => {
            if (!visited(x.to)) {
              dfsUtil(x.to, visited, st, adjacency)
            }
          })
        }
        case None =>
      }
      st.push(k)
    }
  }
}
