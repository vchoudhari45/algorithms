package com.vc.tutorial.algo.graph

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * https://www.youtube.com/watch?v=GiN3jRdgxU4
  */
object FordFulkeronAndEdmondKarpAlgorithm {

  def main(args: Array[String]): Unit = {
    val g = new Graph(7, true)
    //A 0
    //B 1
    //C 2
    //D 3
    //E 4
    //F 5
    //G 6

    //AB 0 1 3
    //CA 2 0 3
    //BC 1 2 4
    //CD 2 3 1
    //CE 2 4 2
    //EB 4 1 1
    //AD 0 3 3
    //DF 3 5 6
    //FG 5 6 9
    //EG 4 6 1
    //DE 3 4 2

    g.addEdge(0, 1, 3)
    g.addEdge(2, 0, 3)
    g.addEdge(1, 2, 4)
    g.addEdge(2, 3, 1)
    g.addEdge(2, 4, 2)
    g.addEdge(4, 1, 1)
    g.addEdge(0, 3, 3)
    g.addEdge(3, 5, 6)
    g.addEdge(5, 6, 9)
    g.addEdge(4, 6, 1)
    g.addEdge(3, 4, 2)
    g.getMaxFlow
  }

  case class Edge(to: Int, var capacity: Int)

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

    def getMaxFlow: Unit = {
      var isPathAvail = true
      var maxFlow = 0
      while (isPathAvail) {
        val visited = Array.fill(n)(false)
        val q = new mutable.Queue[Int]()
        val parent = new mutable.HashMap[Int, (Int, Int)]()

        val source = 0
        val sink = 6
        q.enqueue(source)
        parent.put(source, (Integer.MAX_VALUE, Integer.MAX_VALUE))

        isPathAvail = false
        import scala.util.control.Breaks._
        breakable {
          while (q.nonEmpty) {
            val ele = q.dequeue
            Option(adj(ele)) match {
              case Some(list) => {
                list.foreach(x => {
                  if (!visited(x.to) && x.capacity > 0) {
                    if (x.to == sink) {
                      isPathAvail = true
                      parent.put(x.to, (ele, x.capacity))

                      var p = x.to
                      var min = Integer.MAX_VALUE
                      while (p != source) {
                        val t = parent(p)
                        p = t._1
                        min = Math.min(t._2, min)
                      }

                      maxFlow += min
                      p = x.to
                      print(p + " <- ")
                      while (p != source) {
                        val t = parent(p)
                        print(t._1 + " <- ")

                        //Do minus from capacity on the path
                        adj(t._1).foreach(ele => {
                          if (ele.to == p) ele.capacity = ele.capacity - min
                        })

                        //Add capacity in reverse direction
                        Option(adj(p)) match {
                          case Some(l) => {
                            l.foreach(ele => {
                              if (ele.to == t._1) ele.capacity = ele.capacity + min
                            })
                          }
                          case None => {
                            val l = new ListBuffer[Edge]()
                            l += Edge(t._1, t._2)
                            adj(p) = l
                          }
                        }

                        p = t._1
                      }
                      println("Start")
                      parent.clear
                      q.clear
                      break
                    }
                    visited(x.to) = true
                    q.enqueue(x.to)
                    parent.put(x.to, (ele, x.capacity))
                  }
                })
              }
              case None =>
            }
          }
        }
      }
      println(maxFlow)
    }
  }
}
