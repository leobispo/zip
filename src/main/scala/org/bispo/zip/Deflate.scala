package org.bispo.zip

import org.bispo.zip.Huffman._

import scala.collection.mutable.HashMap
import scala.collection.immutable.SortedMap
import scala.collection.mutable.ListBuffer

class BitStreamWriter() {
  private[this] val arrayList         = new ListBuffer[Byte]
  private[this] var remainingBits     = 32
  private[this] var currentBytes: Int = 0
  private[this] val bb                = java.nio.ByteBuffer.allocateDirect(4)

  def write(bytes: Int, btCnt: Int) : Unit = {
    if (btCnt > 0 && btCnt <= 32) {
      var cleanBytes = bytes & (0xffffffff >>> (32 - btCnt))

      val dx = remainingBits - btCnt
      if (dx < 0) {
        currentBytes = currentBytes | (cleanBytes << (remainingBits - (btCnt + dx)))

        bb.putInt(currentBytes); bb.flip();for (i <- 0 until 4) arrayList += bb.get();bb.clear()

        currentBytes  = 0; remainingBits = 32 + (btCnt + dx)

        cleanBytes = bytes & (0xffffffff >>> (32 - (btCnt + dx)))
      }

      remainingBits = remainingBits - btCnt
      currentBytes = currentBytes | (cleanBytes << (remainingBits))

      if (remainingBits == 0) {
        bb.putInt(currentBytes); bb.flip();for (i <- 0 until 4) arrayList += bb.get();bb.clear()

        currentBytes  = 0; remainingBits = 32
      }
    }
    //THROW AN ERROR!
  }

  def write(bits: List[Boolean]) : Unit = {
    bits foreach { bit => write(if (bit == false) 0 else 1, 1) }
  }

  def flush() : List[Byte] = {
    if (remainingBits != 32) {
      bb.putInt(currentBytes); bb.flip();for (i <- 0 until (4 - (remainingBits / 8))) arrayList += bb.get();bb.clear()
      currentBytes = 0; remainingBits = 32
    }

    val array = arrayList.toList;arrayList.clear

    array
  }
}

class BitStreamReader(val arrayList: Array[Byte]) {
  private var arrayPos = 0
  private var bytePos  = 0
  private var currentByte = if (arrayList.length == 0) 0 else arrayList(0)

  def read(btCnt: Int) : Byte = {
    if (btCnt > 0 && btCnt <= 8) {

    }

    'a' //TODO: TROW AN EXCEPTION
  }

  def reset() : Unit = {
    arrayPos = 0
    bytePos  = 0
  }
}

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
    val table = huffmanTable(bytes) //TODO: Must get it from the Byte array
    val tree = reconstructTree(table)
    null
  }
}
