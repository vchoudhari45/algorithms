package com.vc.tutorial.algo.btree

/**
  * https://www.youtube.com/watch?v=13m9ZCB8gjw
  */
object LowestCommonAncestorBT {

  def main(args: Array[String]): Unit = {
    val btree = new BST()
    btree.addNode(10)
    btree.addNode(-5)
    btree.addNode(-10)
    btree.addNode(0)
    btree.addNode(5)
    btree.addNode(30)
    btree.addNode(36)
    btree.lowestCommonAncestorBST(-10, 36)
    btree.lowestCommonAncestorBST(-10, 0)
    btree.lowestCommonAncestorBST(0, 36)
    btree.lowestCommonAncestorBST(0, -5)
    btree.lowestCommonAncestorBST(-10, 5)
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

    def lowestCommonAncestorBST(r1: Int, r2: Int): Unit = {
      println(s"$r1 $r2 ${lowestCommonAncestorBSTInner(r1, r2, root)}")

      def lowestCommonAncestorBSTInner(r1: Int, r2: Int, r: Option[Node]): Option[Int] = {
        if (r.isDefined) {
          if (r.get.data == r1) return Option(r1)
          else if (r.get.data == r2) return Option(r2)
          else {
            val left = lowestCommonAncestorBSTInner(r1, r2, r.get.l)
            val right = lowestCommonAncestorBSTInner(r1, r2, r.get.r)
            if (left.isDefined && right.isDefined) return Option(r.get.data)
            else if (left.isDefined && right.isEmpty) return left
            else if (left.isEmpty && right.isDefined) return right
            else return None
          }
        }
        None
      }
    }
  }
}
