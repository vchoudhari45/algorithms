package com.vc.tutorial.algo.btree

import scala.collection.mutable


/**
  * https://www.youtube.com/watch?v=D2bIbWGgvzI
  */
object ReverseLevelOrderTraversal {

  def main(args: Array[String]): Unit = {
    val btree = new BST()
    btree.addNode(10)
    btree.addNode(-5)
    btree.addNode(-10)
    btree.addNode(0)
    btree.addNode(5)
    btree.addNode(30)
    btree.addNode(36)
    btree.lvlOrderReverse(btree.root)
  }

  case class Node(var l: Option[Node], var r: Option[Node], data: Int) {
    override def toString: String = data.toString
  }

  class BST {
    var root: Option[Node] = None

    def addNode(d: Int): Node = {
      val n = Node(None, None, d)
      if (root.isEmpty) {
        root = Option(n)
      }
      else {
        var current = root
        var parent = current
        while (current.isDefined) {
          if (d > current.get.data) {
            parent = current
            current = current.get.r
          }
          else if (d < current.get.data) {
            parent = current
            current = current.get.l
          }
          else {
            parent = current
            current = None
          }
        }

        if (d > parent.get.data) {
          parent.get.r = Option(n)
        }
        else if (d < parent.get.data) {
          parent.get.l = Option(n)
        }
      }
      n
    }


    /**
      *        30
      *    10      36
      * 5     20
      *
      * Queue        Stack
      * 30  --       30
      * None --       None
      * 36  --       36
      * 10  --       10
      * None --       None
      * 20   --       20
      * 5    --        5
      * None
      *
      * 5 20
      * 10 36
      * 30
      **/
    def lvlOrderReverse(r: Option[Node]): Unit = {
      val s = new mutable.Stack[Option[Node]]()
      val q = new mutable.Queue[Option[Node]]()
      if (r.isDefined) {
        q.enqueue(Option(r.get))
        q.enqueue(None)

        while (q.nonEmpty) {
          val e = q.dequeue()
          e match {
            case Some(x) => {
              s.push(e)
              if (x.r.isDefined) q.enqueue(x.r)
              if (x.l.isDefined) q.enqueue(x.l)
            }
            case None => {
              if (q.nonEmpty) {
                q.enqueue(None)
                s.push(None)
              }
            }
          }
        }
      }
      s.foreach({
        case Some(x) => print(x + " ")
        case None => println()
      })
    }

  }
}
