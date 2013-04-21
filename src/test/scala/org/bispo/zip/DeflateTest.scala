import org.scalatest.FlatSpec

import org.bispo.zip.Huffman._
import org.bispo.zip.Deflate._
import org.bispo.zip._

class DeflateTest extends FlatSpec {
  "Deflate Table" should "Contains all valid symbols" in {
    val arr: Array[Char] = Array('h', 'h', 'h', 'h', 'g', 'g', 'g', 'g', 'g', 'e', 'e', 'e', 'e', 'e', 'e'
      , 'e', 'e', 'e', 'e', 'c', 'c', 'c', 'c', 'c', 'c', 'c', 'c', 'c', 'c', 'c', 'd', 'd', 'd', 'd', 'd'
      , 'd', 'd', 'd', 'd', 'd', 'd', 'd', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a'
      , 'b', 'b', 'b', 'b', 'b', 'b', 'b', 'b', 'b', 'b', 'b', 'b', 'b', 'f', 'f', 'f', 'f', 'f', 'f', 'f'
      , 'f', 'f', 'f', 'f', 'f', 'f', 'f', 'f', 'f', 'f', 'f', 'f', 'f', 'f', 'f', 'f', 'f')

    val dtable = huffmanTableToDeflateTable(huffmanTable(arr))
    assert(dtable('a') === List(false, true, false))
    assert(dtable('b') === List(false, true, true))
    assert(dtable('c') === List(true, false, false))
    assert(dtable('d') === List(true, false, true))
    assert(dtable('e') === List(true, true, false))
    assert(dtable('f') === List(false, false))
    assert(dtable('g') === List(true, true, true, false))
    assert(dtable('h') === List(true, true, true, true))
  }
}
