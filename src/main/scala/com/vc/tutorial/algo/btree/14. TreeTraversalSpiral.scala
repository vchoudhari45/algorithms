package com.vc.tutorial.algo.btree

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * https://www.youtube.com/watch?v=vjt5Y6-1KsQ
  */
object TreeTraversalSpiral {

  def main(args: Array[String]): Unit = {
    val btree = new BST()
    btree.addNode(10)
    btree.addNode(-5)
    btree.addNode(-10)
    btree.addNode(0)
    btree.addNode(5)
    btree.addNode(30)
    btree.addNode(36)
    btree.treeTraversalSpiral(btree.root)
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
      *         30
      *    10        36
      * 5     20  33    40
      *
      * isLeft = true
      * Queue         Stack
      * 30  --        30
      * 40           None
      * 33            36
      * 20            10
      * 5
      * None --
      * 10  --
      * 36  --
      *
      *
      **/
    def treeTraversalSpiral(r: Option[Node]): Unit = {
      val q = new ArrayBuffer[Option[Node]]()
      val s = new mutable.Queue[Option[Node]]()
      if (r.isDefined) {
        var isLeft = true
        q.append(r)
        q.append(None)
        while (q.nonEmpty) {
          var e: Option[Node] = None
          if (isLeft) {
            e = q.remove(0)
          }
          else {
            e = q.remove(q.size - 1)
          }
          e match {
            case Some(x) => {
              s.enqueue(e)
              if (isLeft) {
                if (x.l.isDefined) q.append(x.l)
                if (x.r.isDefined) q.append(x.r)
              }
              else {
                if (x.r.isDefined) q.prepend(x.r)
                if (x.l.isDefined) q.prepend(x.l)
              }
            }
            case None => {
              s.enqueue(e)
              isLeft = !isLeft
              if (q.nonEmpty) {
                if (isLeft) {
                  q.append(None)
                }
                else {
                  q.prepend(None)
                }
              }
            }
          }
        }
      }

      s.foreach({
        case Some(x) => {
          print(x.data + " ")
        }
        case None => {
          println()
        }
      })
    }

  }
}
