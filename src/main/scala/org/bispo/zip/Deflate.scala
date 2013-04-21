import org.bispo.zip.Huffman._

package org.bispo.zip {
  object Deflate {
    def compress(bytes: Array[Byte]) : Array[Byte] = {
      val table = huffmanTable(bytes)
      null
    }

    def uncompress(bytes: Array[Byte]) : Array[Byte] = {
      val table = huffmanTable(bytes) //TODO: Must get it from the Byte array
      val tree = reconstructTree(table)
      null
    }
  }
}
