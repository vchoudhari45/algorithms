package com.vc.tutorial.algo.btree

/**
  * https://www.youtube.com/watch?v=ZM-sV9zQPEs
  */
object BinaryTreeTraversal {

  def main(args: Array[String]): Unit = {
    val btree = new BST()
    btree.addNode(10)

    btree.addNode(8)
    btree.addNode(7)
    btree.addNode(9)

    btree.addNode(15)
    btree.addNode(12)
    btree.addNode(16)

    print(s"preOrder: ")
    btree.preOrder(btree.root)
    println()

    print(s"inOrder: ")
    btree.inOrder(btree.root)
    println()

    print(s"postOrder: ")
    btree.postOrder(btree.root)
  }

  case class Node(var r: Option[Node], var l: Option[Node], data: Int)

  class BST {
    var root: Option[Node] = None

    def addNode(d: Int): Node = {
      val n = Node(None, None, d)
      if (root.isEmpty) {
        root = Option(n)
        root.get
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
          n
        }
        else if (d < parent.get.data) {
          parent.get.l = Option(n)
          n
        }
        else {
          parent.get
        }
      }
    }

    /**
      * root left right
      **/
    def preOrder(n: Option[Node]): Unit = {
      if (n.isDefined) {
        print(n.get.data + " ")
        preOrder(n.get.l)
        preOrder(n.get.r)
      }
    }

    /**
      * left root right
      **/
    def inOrder(n: Option[Node]): Unit = {
      if (n.isDefined) {
        inOrder(n.get.l)
        print(n.get.data + " ")
        inOrder(n.get.r)
      }
    }

    /**
      * left right root
      **/
    def postOrder(n: Option[Node]): Unit = {
      if (n.isDefined) {
        postOrder(n.get.l)
        postOrder(n.get.r)
        print(n.get.data + " ")
      }
    }
  }
}
