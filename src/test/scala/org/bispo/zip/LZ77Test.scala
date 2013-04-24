import org.scalatest.FlatSpec

import org.bispo.zip.LZ77._
import org.bispo.zip._

class LZ77Test extends FlatSpec {
  "LZ77 Table" should "Contains all valid information" in {
    val test = Array[Byte]('B', 'l', 'a', 'h', '_', 'b', 'l', 'a', 'h', '_', 'b', 'l', 'a', 'h', '_', 'b', 'l', 'a', 'h', '_', 'b', 'l', 'a', 'h','!', 'l', 'a', 'h', '_')

    val ret = generateDistanceLengthTable(test) 

    assert(ret === List((66,(0,0)), (108,(1,1)), (97,(2,2)), (104,(3,3)), (95,(4,4)), (98,(5,5)), (0,(5,18)), (33,(24,24)), (0,(9,4))))
  }
}
