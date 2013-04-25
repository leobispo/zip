import org.scalatest.FlatSpec

import org.bispo.zip.Huffman._
import org.bispo.zip._

class HuffmanTest extends FlatSpec {
  "Count Frequency" should "be reverse sorted" in {
    val arr: Array[Byte] = Array(1, 1, 3, 5, 7, 1, 3, 1, 5, 7, 5)
    val frequencies = countFrequency(arr)

    assert(frequencies(3).element.get === 1)
    assert(frequencies(3).frequency   === 4)
    assert(frequencies(2).element.get === 5)
    assert(frequencies(2).frequency   === 3)
    assert(frequencies(1).element.get === 3)
    assert(frequencies(1).frequency   === 2)
    assert(frequencies(0).element.get === 7)
    assert(frequencies(0).frequency   === 2)
  }

  "Generate Tree" should "Contains all frequency elements" in {
    val arr: Array[Char] = Array('a', 'b', 'b', 'b', 'b', 'b', 'b', 'c' , 'c', 'c', 'c', 'c', 'c', 'c', 'd', 'd', 'e', 'e', 'e', 'e', 'e',
      'e', 'e', 'e')
    val frequencies = countFrequency(arr)
    val tree        = generateTree(frequencies)

    val in = Tree.levelOrder(tree)

    assert(in.size === 9)
    assert(in(0).element === None)
    assert(in(0).frequency === 24)
    assert(in(1).element === None)
    assert(in(1).frequency === 9)
    assert(in(2).element === None)
    assert(in(2).frequency === 15)
    assert(in(3).element === None)
    assert(in(3).frequency === 3)
    assert(in(4).element.get === 'b')
    assert(in(4).frequency === 6)
    assert(in(5).element.get === 'c')
    assert(in(5).frequency === 7)
    assert(in(6).element.get === 'e')
    assert(in(6).frequency === 8)
    assert(in(7).element.get === 'a')
    assert(in(7).frequency === 1)
    assert(in(8).element.get === 'd')
    assert(in(8).frequency === 2)
  }

  "Huffman Table" should "Contains all valid symbols" in {
    val arr: Array[Char] = Array('a', 'b', 'b', 'b', 'b', 'b', 'b', 'b', 'b', 'c' , 'c', 'c', 'c', 'c', 'c', 'c', 'd', 'd', 'e', 'e', 'e', 'e', 'e',
      'e', 'e', 'e')

    val table = huffmanTable(arr)
    assert(table('a') === List(false, false, false))
    assert(table('b') === List(true, true))
    assert(table('c') === List(false, true))
    assert(table('d') === List(false, false, true))
    assert(table('e') === List(true, false))
  }
}
