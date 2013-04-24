package org.bispo.zip

import scala.language.postfixOps
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Queue
import org.bispo.zip._

class HuffmanFrequency[T](var frequency: Int, var element: Option[T] = None) extends Ordered[HuffmanFrequency[T]] {
  def compare(that: HuffmanFrequency[T]) = this.frequency - that.frequency

  override def toString() : String = {
    "H(element: " + element + " frequency: " + frequency + ")"
  }
}

object Huffman {
  def countFrequency[T](arr: Array[T]) : List[HuffmanFrequency[T]] = {
    val hash = HashMap.empty[T, Int] withDefaultValue 0
    arr foreach { hash(_) += 1 }

    val list = new ListBuffer[HuffmanFrequency[T]]
    hash foreach { keyVal => list += new HuffmanFrequency[T](keyVal._2, Option(keyVal._1)) }
    list.sortBy(_.frequency).toList
  }

  def generateTree[T](freqList: List[HuffmanFrequency[T]]) : Option[Node[HuffmanFrequency[T]]] = {
    if (freqList.isEmpty)
      None
    else {
      var treeList = new ListBuffer[Option[Node[HuffmanFrequency[T]]]]
      freqList foreach { element => treeList += Option(new Node[HuffmanFrequency[T]](element)) }

      def generateTreePriv() : Unit = {
        if (treeList.size > 1) {
          val opt1 = treeList(0)
          val opt2 = treeList(1)
          treeList.remove(0, 2)

          val root = Tree.addNode(None,  Option(new Node[HuffmanFrequency[T]](new HuffmanFrequency[T](
            opt1.get.value.frequency + opt2.get.value.frequency))))
          Tree.addNode(root, opt1)
          Tree.addNode(root, opt2)
          treeList += root

          treeList = treeList.sortBy(_.get.value.frequency)
          generateTreePriv()
        }
      }

      generateTreePriv()
      treeList(0)
    }
  }

  def huffmanTable[T](arr: Array[T]) : Map[T, List[Boolean]] = {
    val tree = generateTree(countFrequency(arr))
    val hash = HashMap.empty[T, List[Boolean]]

    def levelOrder(root: Option[Node[HuffmanFrequency[T]]]) : Unit = {
      val queue = new Queue[Pair[Option[Node[HuffmanFrequency[T]]], ListBuffer[Boolean]]]
      queue.enqueue((root, new ListBuffer[Boolean]))

      while (!queue.isEmpty) {
        val curr = queue.dequeue()

        if (curr._1.get.left == None && curr._1.get.right == None) {
          val list = curr._2.clone
          hash(curr._1.get.value.element.get) = list.toList
        }
        else {
          if (curr._1.get.left != None) {
            val cloneList = curr._2.clone
            cloneList += false
            queue.enqueue((curr._1.get.left, cloneList))
          }
          if (curr._1.get.right != None) {
            val cloneList = curr._2.clone
            cloneList += true
            queue.enqueue((curr._1.get.right, cloneList))
          }
        }
      }
    }

    levelOrder(tree)
    return hash.toMap
  }

  def reconstructTree[T](table: Map[T, List[Boolean]]) : Option[Node[HuffmanFrequency[T]]] = {
    val root = Option(new Node[HuffmanFrequency[T]](new HuffmanFrequency[T](0)))
    table foreach {
      keyValue => {
        var curr = root.get
        keyValue._2 foreach {
          status => {
            if (status) {
              if (curr.right == None)
                curr.right = Option(new Node[HuffmanFrequency[T]](new HuffmanFrequency[T](0, Option(keyValue._1))))
              curr = curr.right.get
            }
            else {
              if (curr.left == None)
                curr.left = Option(new Node[HuffmanFrequency[T]](new HuffmanFrequency[T](0, Option(keyValue._1))))
              curr = curr.left.get
            }
          }
        }  
      }
    }
    root
  }
}
