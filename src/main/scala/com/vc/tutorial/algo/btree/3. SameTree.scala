package com.vc.tutorial.algo.btree

/**
  * https://www.youtube.com/watch?v=ySDDslG8wws
  */
object SameTree {

  def main(args: Array[String]): Unit = {
    val btree = new BST()
    btree.addNode(10)
    btree.addNode(-5)
    btree.addNode(-10)
    btree.addNode(0)
    btree.addNode(5)
    btree.addNode(30)
    btree.addNode(36)

    val btree1 = new BST()
    btree1.addNode(10)
    btree1.addNode(-5)
    btree1.addNode(-10)
    btree1.addNode(0)
    btree1.addNode(5)
    btree1.addNode(30)
    btree1.addNode(36)

    println(BST.sameBtree(btree.root, btree1.root))
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
  }

  object BST {
    def sameBtree(r1: Option[Node], r2: Option[Node]): Boolean = {
      if (r1.isEmpty && r2.isEmpty) return true
      if (r1.isEmpty || r2.isEmpty) return false
      r2.get.data == r1.get.data &&
        sameBtree(r1.get.r, r2.get.r) &&
        sameBtree(r1.get.l, r2.get.l)
    }
  }
}
