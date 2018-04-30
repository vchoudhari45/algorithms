package com.vc.tutorial.algo.btree

/**
  * https://www.youtube.com/watch?v=NA8B84DZYSA
  */
object SizeOfBST {

  def main(args: Array[String]): Unit = {
    val btree = new BST()
    btree.addNode(10)
    btree.addNode(-5)
    btree.addNode(-10)
    btree.addNode(0)
    btree.addNode(5)
    btree.addNode(30)
    btree.addNode(36)
    println(s"Size: ${btree.getSize(btree.root)}")
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

    def getSize(r: Option[Node]): Int = {
      if (r.isEmpty) 0
      else 1 + getSize(r.get.l) + getSize(r.get.r)
    }
  }
}
