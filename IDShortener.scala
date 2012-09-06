import _root_.scala.collection.mutable.ArrayBuffer

class IDShortener {
  private val BASE = 62
  def shorten(id : Long) : String = {
    val buffer = new ArrayBuffer[Byte]
    var decimal = id
    def mod(decimal : Long) : Int = (decimal % BASE).toInt
    if (decimal == 0) buffer += map(mod(0))
    else if (decimal < 0) return ""
    else while (decimal != 0) {
      buffer += map(mod(decimal))
      decimal /= BASE
    }
    new String(buffer.reverse.toArray, "US-ASCII")
  }
  def map(index : Int) : Byte = ascii_map(shuffle_map(index)).toByte
  private val shuffle_map : Array[Int] =
    Array(20,41,56,27,6,58,22,34,15,42,28,44,37,61,17,33,14,55,3,60,18,11,48,59,52,21,47,35,1,36,46,2,13,30,4,45,31,7,9,49,57,40,8,54,19,32,29,10,26,25,51,39,53,24,38,16,12,50,5,0,23,43)
//  private val shuffle_map : Array[Int] =
//    List(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61)
  private val ascii_map : Array[Int] =
    Array(65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,
    48,49,50,51,52,53,54,55,56,57)
}