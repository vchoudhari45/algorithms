package com.vc.tutorial.algo.btree

import scala.collection.mutable.ListBuffer

/**
  * https://www.youtube.com/watch?v=Jg4E4KZstFE
  */
object RootToLeafSumBST {

  def main(args: Array[String]): Unit = {
    val btree = new BST()
    btree.addNode(10)
    btree.addNode(-5)
    btree.addNode(-10)
    btree.addNode(0)
    btree.addNode(5)
    btree.addNode(30)
    btree.addNode(36)
    val list = new ListBuffer[Int]()
    btree.getSum(btree.root, 10, list)
    println(list.mkString(" "))
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

    def getSum(r: Option[Node], sum: Int, list: ListBuffer[Int]): Boolean = {
      if (r.isEmpty) false
      else if (r.get.l.isEmpty && r.get.r.isEmpty) {
        if (sum == r.get.data) {
          list += r.get.data
          true
        }
        else {
          false
        }
      }
      else {
        if (getSum(r.get.l, sum - r.get.data, list)) {
          list += r.get.data
          return true
        }
        if (getSum(r.get.r, sum - r.get.data, list)) {
          list += r.get.data
          return true
        }
        false
      }
    }
  }
}
