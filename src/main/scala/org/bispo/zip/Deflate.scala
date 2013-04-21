import org.bispo.zip.Huffman._

import scala.collection.mutable.HashMap
import scala.collection.immutable.SortedMap
import scala.collection.mutable.ListBuffer

package org.bispo.zip {
  object Deflate {
  def huffmanTableToDeflateTable[T <% Ordered[T]](htable: Map[T, List[Boolean]]) : Map[T, List[Boolean]] = {
    val blCount    = HashMap.empty[Int, Int] withDefaultValue 0
    val finalTable = HashMap.empty[T, List[Boolean]]

    var maxBits = 0
    htable foreach { keyValue => blCount(keyValue._2.length) += 1; maxBits = math.max(keyValue._2.length, maxBits) }

    val nextCode = new ListBuffer[Int]
    nextCode += 0

    var code = 0
    for (bits <- 1 to maxBits) {
      nextCode += 0
      code = (code + blCount(bits - 1)) << 1;
      nextCode(bits) = code
    }

    val t = SortedMap(htable.toSeq:_*)
    t foreach {
      keyValue => {
        val len = keyValue._2.length
        if (len != 0) {
          var code = nextCode(len)
          val codeList = new ListBuffer[Boolean]
          keyValue._2 foreach { dummy =>
            if ((code & 1) == 1) codeList.+=:(true) else codeList.+=:(false)
            code = code >> 1
          }

          finalTable(keyValue._1) = codeList.toList 
          nextCode(len) = nextCode(len) + 1
        }
      }
    }

    finalTable.toMap
  }

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
