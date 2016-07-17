package save_prisoner

import java.util.Scanner
import scala.collection.mutable.ArrayBuffer

class Common {
  def collectInt(sc: Scanner, num:Int) = {
    val v = new ArrayBuffer[Int](num)
    while (v.size < num) {
      v += sc.nextInt()
    }
    v
  }
}

object Solution extends Common {
  def findPrisoner(a:Seq[Int]) = {
    val N = a(0)
    val M = a(1)
    val S = a(2)
    val r = (M + S - 1) % N
    val rr = if (r == 0) N else r
    println(s"$rr")
  }

  def main(args: Array[String]) {
    val sc = new java.util.Scanner (System.in)
    val numTests = sc.nextInt
    var i = 0
    while (i < numTests) {
      findPrisoner(collectInt(sc, 3))
      i += 1
    }
  }
}
