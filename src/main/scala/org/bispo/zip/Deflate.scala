package org.bispo.zip

import org.bispo.zip.Huffman._

import scala.collection.mutable.HashMap
import scala.collection.immutable.TreeMap
import scala.collection.mutable.ListBuffer

object Deflate {
def huffmanTableToDeflateTable[T <% Ordered[T]](htable: Map[T, List[Boolean]]) : TreeMap[T, List[Boolean]] = {
  val blCount    = HashMap.empty[Int, Int] withDefaultValue 0
  var finalTable = TreeMap.empty[T, List[Boolean]]

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

  TreeMap(htable.toSeq:_*) foreach {
    keyValue => {
      val len = keyValue._2.length
      if (len != 0) {
        var code = nextCode(len)
        val codeList = new ListBuffer[Boolean]
        keyValue._2 foreach { dummy =>
          if ((code & 1) == 1) codeList.+=:(true) else codeList.+=:(false)
          code = code >> 1
        }

        finalTable += (keyValue._1 -> codeList.toList)
        nextCode(len) = nextCode(len) + 1
      }
    }
  }

  finalTable
}

def compress(bytes: Array[Byte], last:Boolean = false) : List[Byte] = {
    val table = huffmanTable(bytes)
    val stream = new BitStreamWriter

    if (last == true)
      stream.write(1, 1);
    else
      stream.write(0, 1);

    stream.write(2, 2);

    stream.flush()
  }

  def uncompress(bytes: Array[Byte]) : Array[Byte] = {
    null
  }
}
