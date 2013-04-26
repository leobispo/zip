package org.bispo.zip

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
