package qheap

import java.util.Scanner

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

class Common {
  def collectLong(sc: Scanner, num:Int) = {
    val v = new ArrayBuffer[Long](num)
    while (v.size < num) {
      v += sc.nextLong()
    }
    v
  }
}

case class Heap(capacity:Int) {
  val arr = new ArrayBuffer[Long](capacity)
  def child(i:Int) = 2*i + 1
  def parent(i:Int) = Math.floor((i-1)/2).toInt
  def add(x:Long) = {
    val i = arr.size
    arr += x
    shiftUp(i)
  }

  def delete(x:Long) = {
    var i = 0
    while (i < arr.size) {
      if (arr(i) == x) {
        _pop(i)
        i = arr.size
      }
      i += 1
    }
  }

  def pop():Long = _pop(0)

  def _pop(i:Int):Long = {
    if (i == arr.size - 1) {
      arr.remove(i)
    } else {
      swap(i, arr.size - 1)
      val r = arr.remove(arr.size - 1)
      shiftDown(i)
      r
    }
  }

  def swap(x:Int, y:Int) {
    val z = arr(x)
    arr(x) = arr(y)
    arr(y) = z
  }

  @tailrec private def shiftUp(i:Int) {
    if (i == 0) return
    val p = parent(i)
    if (arr(i) < arr(p)) {
      swap(i, p)
      shiftUp(p)
    }
  }

  @tailrec private def shiftDown(i:Int) {
    if (i == arr.size - 1) return
    val c = child(i)
    var target = c

    if (c >= arr.size) return
    if (c == arr.size - 1) {
      target = c
    } else {
      /* c < arr.size - 1 */
      target = if (arr(c) < arr(c+1)) c else c + 1
    }

    if (arr(i) > arr(target)) {
      swap(i, target)
      shiftDown(target)
    }
  }

  def checkMin() = {
    var i = 0
    var m = arr(0)
    while (i < arr.size) {
      if (arr(i) < m) {
        m = arr(i)
      }
      i += 1
    }
    m == min()
  }

  def min() = arr(0)
}

object TestHeap extends App {
  val h = new Heap(10)
  h.add(5)
  h.add(2)
  h.add(3)
  h.add(6)
  assert(h.min == 2)
  h.add(1)
  assert(h.min == 1)
  assert(h.pop == 1)
  assert(h.min == 2)
  h.delete(6)
  h.delete(2)
  assert(h.pop == 3)
  assert(h.pop == 5)
}

object Solution extends Common {
  val sc = new java.util.Scanner (System.in)
  val numQueries = sc.nextInt()
  val h = new Heap(numQueries)
  var p = 0

  def processQuery() = {
    p += 1
    sc.nextLong match {
      case 1 => h.add(sc.nextLong())
      case 2 => h.delete(sc.nextLong())
      case 3 => println(h.min())
    }
    assert(h.checkMin, s"q#:$p")
  }


  def main(args: Array[String]): Unit = {
    var i = 0
    while (i < numQueries) {
      processQuery()
      i += 1
    }
  }
}
