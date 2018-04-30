package com.vc.tutorial.algo.btree

import scala.annotation.tailrec

/**
  * https://www.youtube.com/watch?v=TIoCCStdiFo
  */
object LowestCommonAncestorBST {

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
    btree.lowestCommonAncestorBST(-5, 0)
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

      @tailrec def lowestCommonAncestorBSTInner(r1: Int, r2: Int, r: Option[Node]): Int = {
        if (r.isDefined && r.get.data >= r1 && r.get.data <= r2) r.get.data
        else if (r.get.data >= r1 && r.get.data >= r2) {
          lowestCommonAncestorBSTInner(r1, r2, r.get.l)
        }
        else {
          lowestCommonAncestorBSTInner(r1, r2, r.get.r)
        }
      }
    }
  }
}
