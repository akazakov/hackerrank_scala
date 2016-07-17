package compare_triplets

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
  def computeScore(a:Seq[Int], b:Seq[Int]) = {
    val p = a.zip(b)
    val as = p.map(x => if (x._1 > x._2) 1 else 0).sum
    val bs = p.map(x => if (x._1 < x._2) 1 else 0).sum
    println(s"$as $bs")
  }

  def main(args: Array[String]) {
    val sc = new java.util.Scanner (System.in);
    val a = collectInt(sc, 3)
    val b = collectInt(sc, 3)
    computeScore(a, b)

  }
}
