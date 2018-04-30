package com.vc.tutorial.algo.btree

/**
  * https://www.youtube.com/watch?v=4fiDs7CCxkc
  */
object MaxBST {

  def main(args: Array[String]): Unit = {
    val btree = new BST()
    btree.addNode(10)
    btree.addNode(-5)
    btree.addNode(-10)
    btree.addNode(0)
    btree.addNode(5)
    btree.addNode(30)
    btree.addNode(36)
    println(btree.maxBST(btree.root))
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

    def maxBST(r: Option[Node]): Int = {
      def maxBSTInner(r: Option[Node]): (Int, Int, Int, Boolean) = {
        if (r.isDefined) {
          val left = maxBSTInner(r.get.l)
          val right = maxBSTInner(r.get.r)
          if (!left._4 || !right._4 || r.get.data < left._2 || r.get.data > right._1) {
            return (Int.MinValue, Int.MaxValue, Math.max(left._3, right._3), false)
          }
          return (
            if (r.get.l.isDefined) left._1 else r.get.data,
            if (r.get.r.isDefined) right._2 else r.get.data,
            1 + left._3 + right._3,
            true
          )
        }
        (Int.MaxValue, Int.MinValue, 0, true)
      }

      maxBSTInner(r)._3
    }

  }
}

