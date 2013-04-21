import org.bispo.zip.Huffman._

object Zip {
  def main(args: Array[String]) {
    val arr: Array[Byte] = Array(1, 1, 3, 5, 7)
    println(huffmanTable(arr))
  }
}
