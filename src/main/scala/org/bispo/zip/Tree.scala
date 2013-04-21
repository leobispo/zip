import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Queue

package org.bispo.zip {
  case class Node[T <% Ordered[T]](value: T, var left: Option[Node[T]] = None, var right: Option[Node[T]] = None) {
    override def toString() :String = {
      "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
    }
  }

  object Tree {
    def addNode[T <% Ordered[T]](root: Option[Node[T]], value: T) : Option[Node[T]] = {
      root match {
        case None => Option(new Node[T](value, None, None))
        case Some(curr) => {
          curr.value match {
            case v if v < value => curr.right = addNode(curr.right, value); root
            case v if v >= value => curr.left = addNode(curr.left, value); root
          }
        }
      }
    }

    def addNode[T <% Ordered[T]](root: Option[Node[T]], node: Option[Node[T]]) : Option[Node[T]] = {
      if (node == None)
        root
      else {
        root match {
          case None => node
          case Some(curr) => {
            if (curr.left == None)
              curr.left = node
            else
              curr.right = node

            root
          }
        }
      }
    }

    def inOrder[T <% Ordered[T]](root: Option[Node[T]]) : List[T] = {
      var list = new ListBuffer[T]
      def inOrderPriv(r: Option[Node[T]]) : Unit = {
        r match {
          case Some(curr) => {
            inOrderPriv(curr.left)
            list += curr.value
            inOrderPriv(curr.right)
          }
          case _ =>;
        }
      }
      inOrderPriv(root)
      list.toList
    }

    def levelOrder[T <% Ordered[T]](root: Option[Node[T]]) : List[T] = {
      var list = new ListBuffer[T]

      val queue = new Queue[Option[Node[T]]]
      queue.enqueue(root)
      while (!queue.isEmpty) {
        val curr = queue.dequeue()

        list += curr.get.value
        if (curr.get.left != None)
          queue.enqueue(curr.get.left)
        if (curr.get.right != None)
          queue.enqueue(curr.get.right)
      }

      list.toList
    }
  }
}
