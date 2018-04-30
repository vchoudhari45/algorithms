package com.vc.tutorial.algo.btree

/**
  * https://www.youtube.com/watch?v=wGXB9OWhPTg
  */
object MorrisInorderTraversal {

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
      *           30
      *      20        36
      *   15   24
      *
      **/
    def morrisInorderTraversal(r: Option[Node]): Unit = {
      var current = root
      while(current.isDefined){
        if(current.get.l.isEmpty){
          print(current.get.data+" ")
          current = current.get.r
        }
        else{
          var e = current.get.l
          while(e.get.r.isDefined && e.get.r != current){
            e = e.get.r
          }
          if(e.get.r.isEmpty){
            e.get.r = current
            current = current.get.l
          }
          else{
            e.get.r = None
            print(current.get.data+" ")
            current = current.get.r
          }
        }
      }
    }
  }
  def main(args: Array[String]): Unit = {
    val btree = new BST()
    btree.addNode(30)
    btree.addNode(20)
    btree.addNode(36)
    btree.addNode(15)
    btree.addNode(24)
    btree.morrisInorderTraversal(btree.root)
  }
}