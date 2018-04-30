package com.vc.tutorial.algo.btree

import scala.collection.mutable

/**
  * https://www.youtube.com/watch?v=7uG0gLDbhsI
  */
object LevelByLevelTraversal {

  def main(args: Array[String]): Unit = {
    val btree = new BST()
    btree.addNode(10)
    btree.addNode(-5)
    btree.addNode(-10)
    btree.addNode(0)
    btree.addNode(5)
    btree.addNode(30)
    btree.addNode(36)
    btree.lvlByLvl(btree.root)
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

    def lvlByLvl(r: Option[Node]): Unit = {
      val q = new mutable.Queue[Option[Node]]()
      var current: Option[Node] = None
      if (r.isDefined) {
        q.enqueue(r)
        q.enqueue(None)
        while (q.nonEmpty) {
          current = q.dequeue()
          current match {
            case Some(x) => {
              print(x.data + " ")
              if (x.l.isDefined) q.enqueue(x.l)
              if (x.r.isDefined) q.enqueue(x.r)
            }
            case None => {
              if (q.nonEmpty) {
                q.enqueue(None)
                println()
              }
            }
          }
        }
      }
    }
  }
}
