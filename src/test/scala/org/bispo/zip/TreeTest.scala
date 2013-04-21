import org.scalatest.FlatSpec

import org.bispo.zip._

class TreeTest extends FlatSpec {
  "Test add" should "print all data in order" in {
    class NodeValue(var value: Int) extends Ordered[NodeValue] {
      def compare(that: NodeValue) = this.value - that.value
    }

    val root = Tree.addNode(None, 15)
    Tree.addNode(root, 10)
    Tree.addNode(root, 12)
    Tree.addNode(root, 3)

    val expect = List(3, 10, 12, 15)

    assert(Tree.inOrder(root) === expect)
  }
}
