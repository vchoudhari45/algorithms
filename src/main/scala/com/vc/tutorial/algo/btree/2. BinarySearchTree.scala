package com.vc.tutorial.algo.btree

/**
  * https://www.youtube.com/watch?v=zm83jPHZ-jA
  */
object BinarySearchTree {

  def main(args: Array[String]): Unit = {
    val btree = new BST()
    btree.addNode(10)
    btree.addNode(-5)
    btree.addNode(-10)
    btree.addNode(0)
    btree.addNode(5)
    btree.addNode(30)
    btree.addNode(36)

    btree.search(-10)
    btree.search(-11)
    btree.search(10)
    btree.search(18)
    btree.search(30)
  }

  case class Node(var l: Option[Node], var r: Option[Node], data: Int)

  class BST {
    var root: Option[Node] = None

    def addNode(d: Int): Node = {
      val n = Node(None, None, d)
      if (root.isEmpty) {
        root = Option(n)
        n
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
            return current.get
          }
        }
        if (d > parent.get.data) {
          parent.get.r = Option(n)
          n
        }
        else {
          parent.get.l = Option(n)
          n
        }
      }
    }

    def search(d: Int): Unit = {
      var current = root
      import scala.util.control.Breaks._
      breakable {
        while (current.isDefined) {
          if (d > current.get.data) {
            current = current.get.r
          }
          else if (d < current.get.data) {
            current = current.get.l
          }
          else {
            break
          }
        }
      }
      current match {
        case Some(x) => println(s"${x.data}")
        case None => println("Not Found")
      }
    }

  }
}
