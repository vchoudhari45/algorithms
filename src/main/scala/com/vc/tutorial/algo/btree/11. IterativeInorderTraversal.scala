package com.vc.tutorial.algo.btree

import scala.collection.mutable

/**
  * https://www.youtube.com/watch?v=nzmtCFNae9k
  */
object IterativeInorderTraversal {

  def main(args: Array[String]): Unit = {
    val btree = new BST()
    btree.addNode(10)
    btree.addNode(-5)
    btree.addNode(-10)
    btree.addNode(0)
    btree.addNode(5)
    btree.addNode(30)
    btree.addNode(36)
    btree.inOrder(btree.root)
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
      * left root right
      **/
    def inOrder(r: Option[Node]): Unit = {
      val s1 = new mutable.Stack[Node]()
      if (r.isDefined) {
        var node = r
        var break = true
        while (break) {
          while (node.isDefined) {
            s1.push(node.get)
            node = node.get.l
          }
          if (s1.nonEmpty) {
            val e = s1.pop
            print(e.data + " ")
            node = e.r
          }
          else {
            break = false
          }
        }
      }
    }
  }
}
