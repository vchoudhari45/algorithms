package com.vc.tutorial.algo.btree

/**
  * https://www.youtube.com/watch?v=MILxfAbIhrE
  */
object CheckIfBST {

  def main(args: Array[String]): Unit = {
    val btree = new BST()
    btree.addNode(10)
    btree.addNode(-5)
    btree.addNode(-10)
    btree.addNode(0)
    btree.addNode(5)
    btree.addNode(30)
    btree.addNode(36)
    println(btree.checkIfBST(btree.root))
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

    def checkIfBST(r: Option[Node]): Boolean = {
      def checkIfBSTInner(r: Option[Node], min: Int, max: Int): Boolean = {
        if (r.isEmpty) true
        else {
          if (r.get.data > min && r.get.data < max) {
            checkIfBSTInner(r.get.l, min, r.get.data) && checkIfBSTInner(r.get.r, r.get.data, max)
          }
          else {
            false
          }
        }
      }

      checkIfBSTInner(r, Int.MinValue, Int.MaxValue)
    }
  }
}
