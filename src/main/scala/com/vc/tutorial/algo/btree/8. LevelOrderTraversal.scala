package com.vc.tutorial.algo.btree

import scala.collection.mutable

/**
  * https://www.youtube.com/watch?v=9PHkM0Jri_4
  */
object LevelOrderTraversal {

  def main(args: Array[String]): Unit = {
    val btree = new BST()
    btree.addNode(10)
    btree.addNode(-5)
    btree.addNode(-10)
    btree.addNode(0)
    btree.addNode(5)
    btree.addNode(30)
    btree.addNode(36)
    btree.lvlOrderTraversal(btree.root)
  }

  case class Node(var l: Option[Node], var r: Option[Node], data: Int)

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

    def lvlOrderTraversal(r: Option[Node]): Unit = {
      val q = new mutable.Queue[Option[Node]]()
      q.enqueue(r)
      while (q.nonEmpty) {
        val e = q.dequeue()
        e match {
          case Some(x) => {
            print(x.data + " ")
            if (x.l.isDefined) q.enqueue(x.l)
            if (x.r.isDefined) q.enqueue(x.r)
          }
          case None =>
        }
      }
    }
  }
}
